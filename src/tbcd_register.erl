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
-module(tbcd_register).
-author('flygoast@126.com').


-behaviour(gen_server).


-include("tbcd.hrl").


-export([start_link/0,
         register/1,
         register/2,
         unregister/2]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {dict = dict:new()}).


start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


register(Worker) ->
    gen_server:call({global, ?MODULE}, {register, Worker}).


register(Worker, Project) ->
    gen_server:call({global, ?MODULE}, {register, Worker, Project}).


unregister(Worker, Project) ->
    gen_server:call({global, ?MODULE}, {unregister, Worker, Project}).


%%================================================
%% gen_server callbacks
%%================================================
init([]) ->
    {ok, #state{}}.


handle_call({register, Worker}, {Pid, _}, State) ->
    Dict = State#state.dict,

    case dict:find(Worker, Dict) of
    error ->
        ProDict = dict:store(all, Pid, dict:new()),
        NewDict = dict:store(Worker, ProDict, Dict),
        {reply, yes, State#state{dict = NewDict}};
    {ok, ProDict} ->
        case dict:size(ProDict) of
        0 ->
            NewProDict = dict:store(all, Pid, ProDict),
            NewDict = dict:store(Worker, NewProDict, Dict),
            {reply, yes, State#state{dict = NewDict}};
        _ ->
            {reply, other, State}
        end
    end;
handle_call({register, Worker, Project}, {Pid, _}, State) ->
    Dict = State#state.dict,

    case dict:find(Worker, Dict) of
    error ->
        ProDict = dict:store(Project, Pid, dict:new()),
        NewDict = dict:store(Worker, ProDict, Dict),
        {reply, yes, State#state{dict = NewDict}};
    {ok, ProDict} ->
        case dict:find(all, ProDict) of
        error ->
            case dict:find(Project, ProDict) of
            error ->
                NewProDict = dict:store(Project, Pid, ProDict),
                NewDict = dict:store(Worker, NewProDict, Dict),
                {reply, yes, State#state{dict = NewDict}};
            {ok, Pid} ->
                {reply, project, State}
            end;
        {ok, Pid} ->
            {reply, all, State}
        end
    end;
handle_call({unregister, Worker, Project}, {Pid, _}, State) ->
    Dict = State#state.dict,

    case dict:find(Worker, Dict) of
    error ->
        {reply, ok, State};
    {ok, ProDict} ->
        case Project of
        <<"">> ->
            NewDict = dict:erase(Worker, Dict),
            {reply, ok, State#state{dict = NewDict}};
        _ ->
            case dict:find(Project, ProDict) of
            error ->
                {reply, ok, State};
            {ok, Pid} ->
                NewProDict = dict:erase(Project, ProDict),
                NewDict = dict:store(Worker, NewProDict, Dict),
                {reply, ok, State#state{dict = NewDict}}
            end
        end
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
