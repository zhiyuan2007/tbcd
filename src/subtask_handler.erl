%%%
%%% Copyright (c) 2015, Gu Feng <flygoast@126.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%
-module(subtask_handler).
-behaviour(cowboy_loop_handler).
-author('flygoast@126.com').


-export([init/3,
         info/3,
         terminate/3]).


-include("tbcd.hrl").


-record(state, {worker = <<"">> :: binary()}).


init(_, Req, _Opts) ->
    {ok, Body, Req2} = cowboy_req:body(Req),

    lager:info("body: ~p", [Body]),

    case mochijson2:decode(Body) of
    {'EXIT', Error} ->
        lager:info("invalid json: ~p", [Error]),
        Content = mochijson2:encode({struct,
                                     [{<<"code">>, 1},
                                      {<<"reason">>, <<"invalid json">>}]}),
        shutdown_json(Req2, 200, Content);
    {struct, Ls} ->
        case lists:keyfind(<<"mode">>, 1, Ls) of
        false ->
            lager:info("mode argument not supplied", []),
            Content = mochijson2:encode({struct,
                                         [{<<"code">>, 1},
                                          {<<"reason">>,
                                           <<"mode argument not supplied">>}]}),
            shutdown_json(Req2, 200, Content);
        {_, <<"feedback">>} ->
            Content = subtask_feedback(Ls),
            shutdown_json(Req2, 200, Content);
        {_, <<"fetch">>} ->
            case lists:keyfind(<<"worker">>, 1, Ls) of
            false ->
                Content = mochijson2:encode({struct,
                                             [{<<"code">>, 1},
                                              {<<"reason">>,
                                               <<"worker argument "
                                                 "not supplied">>}]}),
                shutdown_json(Req2, 200, Content);
            {_, W} ->
                case subtask_fetch(W) of
                {aborted, _Reason} ->
                    Content = <<"Internal server error">>,
                    shutdown_plain(Req2, 500, Content);
                [] ->
                    %% now no subtask
                    case global:register_name(W, self()) of
                    yes ->
                        %% wait for subtasks arrived or timeout
    	                {loop, Req2, #state{worker = W}, 30000, hibernate};
                    no ->
                        Content = mochijson2:encode({struct,
                                                     [{<<"code">>, 1},
                                                      {<<"reason">>,
                                                       <<"worker been "
                                                         "registered">>}]}),
                        shutdown_json(Req2, 200, Content)
                    end;
                T ->
                    Content = mochijson2:encode({struct,
                                                 [{<<"code">>, 0},
                                                  {<<"subtasks">>, T}]}),
                    shutdown_json(Req2, 200, Content)
                end
            end;
        _ ->
            Content = mochijson2:encode({struct,
                                         [{<<"code">>, 1},
                                          {<<"reason">>,
                                           <<"invalid mode argument">>}]}),
            shutdown_json(Req2, 200, Content)
        end
    end.


info({reply}, Req, State) ->
    Worker = State#state.worker,

    case subtask_fetch(Worker) of
    {aborted, _Reason} ->
        Content = <<"Internal server error">>,
        tbcd_reply:reply_plain(Req, State, 200, Content);
    [] ->
        lager:error("no subtask fetched: ~p", [Worker]),
        Content = <<"Internal server error">>,
        tbcd_reply:reply_plain(Req, State, 200, Content);
    T ->
        Content = mochijson2:encode({struct,
                                     [{<<"code">>, 0},
                                      {<<"subtasks">>, T}]}),
        lager:info("fetched: ~p", [Content]),
        tbcd_reply:reply_json(Req, State, 200, Content)
    end;
info(_Info, Req, State) ->
	{loop, Req, State, hibernate}.


terminate(_Reason, _Req, _State) ->
	ok.


subtask_feedback(Ls) ->
    case lists:keyfind(<<"worker">>, 1, Ls) of
    false ->
        mochijson2:encode({struct,
                           [{<<"code">>, 1},
                            {<<"reason">>,
                             <<"worker argument not supplied">>}]});
    {_, W} ->
        case lists:keyfind(<<"subtasks">>, 1, Ls) of
        false ->
            mochijson2:encode({struct,
                               [{<<"code">>, 1},
                                {<<"reason">>,
                                 <<"subtasks argument not supplied">>}
                               ]});
        {_, S} ->
            case subtask_feedback(W, S) of
            [] ->
                mochijson2:encode({struct,
                                   [{<<"code">>, 0}]});
            T ->
                mochijson2:encode({struct,
                                   [{<<"code">>, 1},
                                    {<<"subtasks">>, T}]})
            end
        end
    end.


subtask_feedback(Worker, Subtasks) ->
    F = fun([Id, Result]) ->
            F2 = fun() ->
                     T = #subtask{sid = {Id, Worker},
                                  timestamp = now(),
                                  result = Result},
                     CounterIncr = case mnesia:read(finished_subtask,
                                              T#subtask.sid) of
                                   [] -> -1;
                                   _ -> 0
                                   end,
                     mnesia:write(finished_subtask, T, write),
                     mnesia:delete({fetched_subtask, T#subtask.sid}),
                     CounterIncr
                 end,
            case mnesia:transaction(F2) of
            {atomic, CounterIncr} ->
                tbcd_subtask:get_proc() ! {feedback, Id, CounterIncr},
                false;
            {aborted, Reason} ->
                lager:error("feedback, mnesia error: ~p", [Reason]),
                {true, Id}
            end
        end,
    lists:filtermap(F, Subtasks).


subtask_fetch(Worker) ->
    F = fun() ->
            MatchHead = #subtask{sid = {'$1', Worker}, _ = '_'},
            Guard = [],
            Result = '$_',
            case mnesia:select(unfetched_subtask,
                               [{MatchHead, Guard, [Result]}],
                               10,
                               read) of
            {Es, _Cont} ->
                F2 = fun(E) ->
                         {Tid, _} = E#subtask.sid,
                         [Task] = mnesia:read(task, Tid),

                         mnesia:write(fetched_subtask, E, write),
                         mnesia:delete({unfetched_subtask, E#subtask.sid}),

                         lager:info("fetched task: ~p", [Task]),

                         [Tid, Task#task.project, Task#task.content]
                     end,
                lists:map(F2, Es);
            '$end_of_table' ->
                []
            end
        end,
    case mnesia:transaction(F) of
    {atomic, Rs} ->
        lager:info("fetched success: ~p", [Rs]),
        Rs;
    {aborted, Reason} ->
        lager:error("mnesia error: ~p", [Reason]),
        {aborted, Reason}
    end.


shutdown_plain(Req, Code, Content) ->
    {ok, Req2} = cowboy_req:reply(Code,
                                  [{<<"content-type">>, <<"text/plain">>},
                                   {<<"connection">>, <<"close">>}
                                  ],
                                  Content, Req),
    {shutdown, Req2, #state{}}.


shutdown_json(Req, Code, Content) ->
    {ok, Req2} = cowboy_req:reply(Code,
                                  [{<<"content-type">>, <<"application/json">>},
                                   {<<"connection">>, <<"close">>}],
                                  Content,
                                  Req),
    {shutdown, Req2, #state{}}.
