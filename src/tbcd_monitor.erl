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
-module(tbcd_monitor).
-author('flygoast@126.com').


-behaviour(gen_server).


-include("tbcd.hrl").


-export([start_link/0]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {timeout = 10 * 60,
                module = tbcd_alarm,
                alarm_state = undefined}).


start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


%%============================================================================
%% gen_server callbacks
%%============================================================================
init([]) ->
    CronInterval = application:get_env(tbcd, cron_interval, 60), %% second
    Timeout = application:get_env(tbcd, subtask_timeout, 10 * 60), %% second
    Module = application:get_env(tbcd, alarm_module, tbcd_alarm),

    AlarmState = (catch erlang:apply(Module, start, [])),

    timer:send_interval(CronInterval * 1000, self(), cron),

    {ok, #state{timeout = Timeout,
                module = Module,
                alarm_state = AlarmState}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(cron, State) ->
    Now = now_to_unix(erlang:now()),

    lager:info("monitor cron: ~p", [Now]),

    NewState = case check_unfetched(State, Now) of
               ok ->
                   State;
               {alarmed, S1} ->
                   State#state{alarm_state = S1}
               end,
    NewState1 = case check_fetched(NewState, Now) of
                ok ->
                    NewState;
                {alarmed, S2} ->
                    NewState#state{alarm_state = S2}
                end,
    {noreply, NewState1};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


now_to_unix(Now) ->
    {M, S, _} = Now,
    M * 1000000 + S.


check_unfetched(State, Now) ->
    #state{timeout = Timeout, module = Module, alarm_state = AS} = State,

    case catch mnesia:dirty_first(unfetched_subtask) of
    {'EXIT', Reason} ->
        lager:error("mnesia failed: ~p", [Reason]),
        ok;
    '$end_of_table' ->
        ok;
    Key ->
        case catch mnesia:dirty_read(unfetched_subtask, Key) of
        {'EXIT', Reason} ->
            lager:error("mnesia failed: ~p", [Reason]),
            ok;
        [R] ->
            #subtask{timestamp = TimeStamp} = R,
            Created = now_to_unix(TimeStamp),

            if
            Now > Created + Timeout ->
                S = (catch erlang:apply(Module, send_alarm,
                                        [AS, unfetched, R, Now])),
                {alarmed, S};
            true ->
                ok
            end
        end
    end.


check_fetched(State, Now) ->
    #state{timeout = Timeout, module = Module, alarm_state = AS} = State,

    case catch mnesia:dirty_first(fetched_subtask) of
    {'EXIT', Reason} ->
        lager:error("mnesia failed: ~p", [Reason]),
        ok;
    '$end_of_table' ->
        ok;
    Key ->
        case catch mnesia:dirty_read(fetched_subtask, Key) of
        {'EXIT', Reason} ->
            lager:error("mnesia failed: ~p", [Reason]),
            ok;
        [R] ->
            #subtask{timestamp = TimeStamp} = R,
            Created = now_to_unix(TimeStamp),

            if
            Now > Created + Timeout ->
                S = (catch erlang:apply(Module, send_alarm,
                                        [AS, fetched, R, Now])),
                {alarmed, S};
            true ->
                ok
            end
        end
    end.
