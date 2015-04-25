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


-record(state, {worker = <<"">> :: binary(),
                project = <<"">> :: binary()}).


init(_, Req, _Opts) ->
    case tbcd_validation:valid_request(Req) of
    {yes, Body, Req2} ->
        handle_request(Req2, Body);
    {no, _, Req2} ->
        shutdown_plain(Req2, 403, <<"access forbidden">>)
    end.


handle_request(Req, Body) ->
    case catch jiffy:decode(Body) of
    {Ls} ->
        case lists:keyfind(<<"mode">>, 1, Ls) of
        false ->
            lager:info("mode argument not supplied", []),
            Content = jiffy:encode({[{<<"code">>, 1},
                                     {<<"reason">>,
                                      <<"mode argument not supplied">>}]}),
            shutdown_json(Req, 200, Content);
        {_, <<"feedback">>} ->
            case subtask_feedback(Ls) of
            {error, Reason} ->
                Content = jiffy:encode({[{<<"code">>, 1},
                                         {<<"reason">>,
                                          list_to_binary(Reason)}]}),
                shutdown_json(Req, 200, Content);
            ok ->
                Content = jiffy:encode({[{<<"code">>, 0}]}),
                shutdown_json(Req, 200, Content)
            end;
        {_, <<"fetch">>} ->
            case lists:keyfind(<<"worker">>, 1, Ls) of
            false ->
                Content = jiffy:encode({[{<<"code">>, 1},
                                         {<<"reason">>,
                                          <<"worker argument not supplied">>}
                                        ]}),
                shutdown_json(Req, 200, Content);
            {_, <<"">>} ->
                Content = jiffy:encode({[{<<"code">>, 1},
                                         {<<"reason">>,
                                          <<"worker argument is empty">>}]}),
                shutdown_json(Req, 200, Content);
            {_, W} when is_binary(W) ->
                case subtask_fetch(Ls, W) of
                {aborted, _Reason} ->
                    Content = <<"Internal server error">>,
                    shutdown_plain(Req, 500, Content);
                {wait, P} ->
                    case tbcd_register:register(W, P) of
                    yes ->
                        %% wait for subtasks arrived or timeout
                        {loop, Req, #state{worker = W, project = P}, 30000,
                         hibernate};
                    project ->
                        Content = jiffy:encode({[{<<"code">>, 1},
                                                 {<<"reason">>,
                                                  <<"worker for this project "
                                                    "been registered">>}
                                                ]}),
                        shutdown_json(Req, 200, Content);
                    all ->
                        Content = jiffy:encode({[{<<"code">>, 1},
                                                 {<<"reason">>,
                                                  <<"worker for all projects "
                                                    "been registered">>}
                                                ]}),
                        shutdown_json(Req, 200, Content)
                    end;
                [] ->
                    case tbcd_register:register(W) of
                    yes ->
                        %% wait for subtasks arrived or timeout
                        {loop, Req, #state{worker = W}, 30000, hibernate};
                    other ->
                        Content = jiffy:encode({[{<<"code">>, 1},
                                                 {<<"reason">>,
                                                  <<"worker been registered">>}
                                                ]}),
                        shutdown_json(Req, 200, Content)
                    end;
                T ->
                    Content = jiffy:encode({[{<<"code">>, 0},
                                             {<<"subtasks">>, T}]}),
                    shutdown_json(Req, 200, Content)
                end;
            _ ->
                Content = jiffy:encode({[{<<"code">>, 1},
                                         {<<"reason">>,
                                          <<"worker argument is not string">>}
                                        ]}),
                shutdown_json(Req, 200, Content)
            end;
        _ ->
            Content = jiffy:encode({[{<<"code">>, 1},
                                      {<<"reason">>,
                                       <<"invalid mode argument">>}]}),
            shutdown_json(Req, 200, Content)
        end;
    {error, Error} ->
        lager:info("invalid json: ~p", [Error]),
        Content = jiffy:encode({[{<<"code">>, 1},
                                 {<<"reason">>, <<"invalid json">>}]}),
        shutdown_json(Req, 200, Content)
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
        Content = jiffy:encode({[{<<"code">>, 0}, {<<"subtasks">>, T}]}),
        lager:info("fetched: ~p", [Content]),
        tbcd_reply:reply_json(Req, State, 200, Content)
    end;
info(_Info, Req, State) ->
    {loop, Req, State, hibernate}.


terminate(_Reason, _Req, #state{worker = Worker, project = Project}) ->
    case Worker of
    <<"">> ->
        null;
    _ ->
        tbcd_register:unregister(Worker, Project)
    end,

    ok.


subtask_feedback(Ls) ->
    case lists:keyfind(<<"worker">>, 1, Ls) of
    false ->
        {error, "worker argument not supplied"};
    {_, <<"">>} ->
        {error, "worker argument is empty"};
    {_, W} when is_binary(W) ->
        case lists:keyfind(<<"subtasks">>, 1, Ls) of
        false ->
            {error, "subtasks argument not supplied"};
        {_, []} ->
            {error, "subtasks argument is empty"};
        {_, S} when is_list(S) ->
            case subtask_feedback(W, S) of
            true ->
                ok;
            {false, Reason} ->
                {error, Reason};
            error ->
                {error, "process failed"};
            _ ->
                {error, "invalid subtask"}
            end;
        _ ->
            {error, "subtasks argument must be a array"}
        end;
    _ ->
        {error, "workers argument is not string"}
    end.


subtask_feedback(_Worker, []) ->
    true;
subtask_feedback(Worker, [{Subtask} | L]) ->
    case lists:keyfind(<<"tid">>, 1, Subtask) of
    false ->
        {false, "tid argument not supplied"};
    {_, <<"">>} ->
        {false, "tid argument is empty"};
    {_, Tid} when is_binary(Tid) ->
        case lists:keyfind(<<"result">>, 1, Subtask) of
        false ->
            {false, "result argument not supplied"};
        {_, Result} ->
            %% Result can be anything
            case subtask_feedback(Worker, Tid, Result) of
            true ->
                subtask_feedback(Worker, L);
            error ->
                error
            end
        end;
    _ ->
        {false, "tid argument is not string"}
    end;
subtask_feedback(_, _) ->
    false.


subtask_feedback(Worker, TidString, Result) ->
    Tid = binary_to_integer(TidString),
    F = fun() ->
            T = #subtask{sid = {Tid, Worker},
                         timestamp = now(),
                         result = Result},

            CounterIncr = case mnesia:wread({finished_subtask,
                                             T#subtask.sid}) of
                          [] -> -1;
                          _ -> 0
                          end,

            mnesia:write(finished_subtask, T, write),
            mnesia:delete({fetched_subtask, T#subtask.sid}),
            CounterIncr
        end,
    case mnesia:transaction(F) of
    {atomic, CounterIncr} ->
        tbcd_subtask ! {feedback, Tid, CounterIncr},
        true;
    {aborted, Reason} ->
        lager:error("feedback, mnesia error: ~p", [Reason]),
        error
    end.


subtask_fetch(Ls, Worker) ->
    case lists:keyfind(<<"project">>, 1, Ls) of
    false ->
        subtask_fetch(Worker);
    {_, <<"">>} ->
        subtask_fetch(Worker);
    {_, Project} when is_binary(Project) ->
        case subtask_fetch_with_project(Worker, Project) of
        [] ->
            {wait, Project};
        R ->
            R
        end
    end.


subtask_fetch_with_project(Worker, Project) ->
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

                         case Task#task.project of
                         Project ->
                             mnesia:write(fetched_subtask, E, write),
                             mnesia:delete({unfetched_subtask, E#subtask.sid}),

                             {true, {[{<<"tid">>, integer_to_binary(Tid)},
                                      {<<"project">>, Task#task.project},
                                      {<<"content">>, Task#task.content}]}};
                         _ ->
                             false
                         end
                     end,
                lists:filtermap(F2, Es);
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

                         {[{<<"tid">>, integer_to_binary(Tid)},
                           {<<"project">>, Task#task.project},
                           {<<"content">>, Task#task.content}]}
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
