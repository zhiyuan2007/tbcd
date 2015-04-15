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
-module(task_handler).
-behaviour(cowboy_http_handler).
-author('flygoast@126.com').


-export([init/3,
         handle/2,
         terminate/3]).


-include("tbcd.hrl").


-record(state, {}).


init(_, Req, _Opts) ->
	{ok, Req, #state{}}.


handle(Req, State=#state{}) ->
    {ok, Body, Req2} = cowboy_req:body(Req),

    lager:info("body: ~p", [Body]),

    case catch jiffy:decode(Body) of
    {Ls} ->
        case lists:keyfind(<<"mode">>, 1, Ls) of
        false ->
            lager:info("mode argument not supplied", []),
            Content = jiffy:encode({[{<<"code">>, 1},
                                     {<<"reason">>,
                                      <<"mode argument not supplied">>}]}),
            tbcd_reply:reply_json(Req2, State, 200, Content);
        {_, <<"add">>} ->
            case task_add(Ls) of
            {aborted, _Reason} ->
                tbcd_reply:reply_plain(Req2, State, 500,
                                       <<"Internal server error">>);
            {error, Reason} ->
                Content = jiffy:encode({[{<<"code">>, 1},
                                         {<<"reason">>,
                                          list_to_binary(Reason)}]}),
                tbcd_reply:reply_json(Req2, State, 200, Content);
            Tid ->
                Content = jiffy:encode({[{<<"code">>, 0},
                                         {<<"tid">>, Tid}]}),
                tbcd_reply:reply_json(Req2, State, 200, Content)
            end;
        {_, <<"select">>} ->
            case task_select(Ls) of
            {error, Reason} ->
                Content = jiffy:encode({[{<<"code">>, 1},
                                         {<<"reason">>,
                                          list_to_binary(Reason)}]}),
                tbcd_reply:reply_json(Req2, State, 200, Content);
            Rs ->
                Content = jiffy:encode({[{<<"code">>, 0}, {<<"tasks">>, Rs}]}),
                lager:info("select result: ~p", [Content]),
                tbcd_reply:reply_json(Req2, State, 200, Content)
            end;
        {_, Mode} ->
            lager:error("invalid mode argument: ~p", [Mode]),
            Content = jiffy:encode({[{<<"code">>, 1},
                                     {<<"reason">>,
                                      <<"invalid mode argument">>}]}),
            tbcd_reply:reply_json(Req2, State, 200, Content)
        end;
    {error, Error} ->
        lager:info("invalid json: ~p", [Error]),
        Content = jiffy:encode({[{<<"code">>, 1},
                                 {<<"reason">>, <<"invalid json">>}]}),
        tbcd_reply:reply_json(Req2, State, 200, Content)
    end.


terminate(_Reason, _Req, _State) ->
	ok.


task_select(Ls) ->
    case lists:keyfind(<<"tids">>, 1, Ls) of
    false ->
        {error, "tids argument not supplied"};
    {_, TIDS} ->
        lager:info("tids: ~p", [TIDS]),
        F = fun(Id) ->
                case mnesia:dirty_read(task_count, Id) of
                [] ->
                    {[{<<"tid">>, Id},
                      {<<"status">>, -1}]};
                [TaskCount] ->
                    #task_count{count = Count} = TaskCount,

                    lager:info("select, tid: ~p, unfinished count: ~p",
                               [Id, Count]),

                    {[{<<"tid">>, Id},
                      {<<"status">>, list_to_binary(integer_to_list(Count))}]}
                end
            end,
        lists:map(F, TIDS)
    end.


task_add(Ls) ->
    Project = case lists:keyfind(<<"project">>, 1, Ls) of
              false ->
                  false;
              {_, P} ->
                  P
              end,

    Content = case lists:keyfind(<<"content">>, 1, Ls) of
              false ->
                  false;
              {_, C} ->
                  C
              end,

    Callback = case lists:keyfind(<<"callback">>, 1, Ls) of
               false ->
                   undefined;
               {_, CL} ->
                   CL
               end,

    lager:info("add: project: ~p, content: ~p, callback: ~p",
               [Project, Content, Callback]),

    case {Project, Content} of
    {false, _} ->
        {error, "project argument not supplied"};
    {<<"">>, _} ->
        {error, "project argument is empty"};
    {_, false} ->
        {error, "content argument not supplied"};
    {_, <<"">>} ->
        {error, "content argument is empty"};
    {_, _} ->
        case mnesia:dirty_read(project, Project) of
        [] ->
            {error, "invalid project"};
        _ ->
            Tid = list_to_binary(uuid:to_string(uuid:uuid1())),

            lager:info("add: taskid: ~p", [Tid]),

            F = fun() ->
                    mnesia:write(#task{tid = Tid,
                                       project = Project,
                                       content = Content,
                                       callback = Callback,
                                       timestamp = now()})
                end,
            case mnesia:transaction(F) of
            {atomic, _Rs} ->
                tbcd_subtask:get_proc() ! {new, Tid, Project},
                Tid;
            {aborted, Reason} ->
                lager:error("add: mnesia error: ~p", [Reason]),
                {aborted, Reason}
            end
        end
    end.
