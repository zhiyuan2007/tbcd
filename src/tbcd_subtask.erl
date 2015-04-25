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
-module(tbcd_subtask).
-author('flygoast@126.com').


-behaviour(gen_server).


-include("tbcd.hrl").


-export([start_link/0,
         http_recv/1]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%================================================
%% gen_server callbacks
%%================================================
init([]) ->
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({new, Tid, Project}, State) ->
    lager:info("project: ~p, tid: ~p", [Project, Tid]),

    F = fun() ->
            case mnesia:read(project, Project) of
            [] ->
                {error, "project not existed"};
            [#project{workers = W}] ->
                L = sets:to_list(W),

                lager:info("project: ~p, workers: ~p", [Project, L]),

                lists:foreach(fun(Ele) ->
                                  ST = #subtask{sid = {Tid, Ele},
                                                timestamp = now()},
                                  mnesia:write(unfetched_subtask, ST, write),

                                  case tbcd_register:registered(Ele, Project) of
                                  undefined ->
                                      ok;
                                  Pid ->
                                      Pid ! {reply}
                                  end
                              end, L),
                length(L)
            end
        end,
    case mnesia:transaction(F) of
    {atomic, {error, Reason}} ->
        lager:error("new subtask error: ~p", [Reason]);
    {atomic, N} ->
        lager:info("new subtask count: ~p", [N]),
        mnesia:dirty_update_counter(task_count, Tid, N);
    {aborted, Reason} ->
        lager:error("mnesia error: ~p", [Reason])
    end,
    {noreply, State};
handle_info({feedback, Tid, Incr}, State) ->
    NewCount = mnesia:dirty_update_counter(task_count, Tid, Incr),

    lager:info("incr: ~p, tid: ~p, count: ~p", [Incr, Tid, NewCount]),

    case NewCount of
    0 ->
        F = fun() ->
                case mnesia:read(task, Tid) of
                [] ->
                    {error, "invalid tid"};
                [#task{callback = undefined}] ->
                    ok;
                [#task{callback = URL}] ->
                    %% get HTTP callback and all results
                    MatchHead = #subtask{sid = {Tid, '$1'},
                                         result = '$2', _ = '_'},
                    Guard = [],
                    R = mnesia:select(finished_subtask,
                                      [{MatchHead, Guard, ['$$']}]),
                    {binary_to_list(URL), R}
                end
            end,
        case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        {atomic, {error, Reason}} ->
            lager:info("feedback, error: ~p", [Reason]),
            ok;
        {atomic, {Callback, Rs}} ->

            Ret = lists:map(fun([Worker, Result]) ->
                                {[{<<"worker">>, Worker},
                                  {<<"result">>, Result}]}
                            end, Rs),

            Content = jiffy:encode({[{<<"tid">>, integer_to_binary(Tid)},
                                     {<<"results">>, Ret}]}),
            Headers = [{"Connection", "close"}],
            Req = {Callback, Headers, "application/json", Content},
            Opts = [{timeout, 10000}, {connect_timeout, 5000}],
            HttpOpts = [{sync, false}, {stream, self},
                        {receiver, {?MODULE, http_recv, []}}],
            {ok, RequestId} = httpc:request(post, Req, Opts, HttpOpts),

            lager:info("callback: ~p, tid: ~p, requestid: ~p",
                       [Callback, Tid, RequestId]);
        {aborted, Reason} ->
            lager:error("mnesia failed: ~p", [Reason])
        end;
    _ ->
        ok
    end,

    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


http_recv({RequestId, {error, Reason}}) ->
    lager:error("http response error:[~p] ~p~n", [RequestId, Reason]),
    ok;
http_recv({RequestId, Result}) ->
    lager:info("http response result:[~p] ~p", [RequestId, Result]),
    ok;
http_recv({RequestId, stream_start, Headers}) ->
    lager:info("http response headers:[~p] ~p", [RequestId, Headers]),
    ok;
http_recv({RequestId, stream, BinBodyPart}) ->
    lager:info("http response body:[~p] ~p", [RequestId, BinBodyPart]),
    ok;
http_recv({RequestId, stream_end, _Headers}) ->
    lager:info("http response body end:[~p]", [RequestId]),
    ok.
