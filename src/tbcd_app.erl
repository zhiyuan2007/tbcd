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
-module(tbcd_app).
-behaviour(application).
-author('flygoast@126.com').


-include("tbcd.hrl").


-export([start/2,
         stop/1]).


start(_Type, _Args) ->
    inets:start(),

    Port = application:get_env(tbcd, port, 8080),
    Host = application:get_env(tbcd, host, '_'),

    lager:info("host:~p, port: ~p", [Host, Port]),

    %% initialized database
    db_init(),

    Dispatch = cowboy_router:compile([{Host, [{"/project", project_handler, []},
                                              {"/task", task_handler, []},
                                              {"/subtask", subtask_handler, []},
                                              {"/[...]", default_handler, []}
                                             ]}
                                     ]),
    tbcd_validation:start(),

    cowboy:start_http(my_http_listener, 100, [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]),

    tbcd_sup:start_link().


stop(_State) ->
    ok.


%%%----------------------------------------------------
%%% Initialized database.
%%%----------------------------------------------------
db_init() ->
    MyNode = node(),
    DbNodes = mnesia:system_info(db_nodes),
    case lists:member(MyNode, DbNodes) of
    true ->
        ok;
    false ->
        lager:critical("node name mismatch: I'm [~p], "
                       "the database is owned by [~p]", [MyNode, DbNodes]),
        erlang:error(node_name_mismatch)
    end,
    case mnesia:system_info(extra_db_nodes) of
    [] ->
        mnesia:create_schema([node()]);
    _ ->
        ok
    end,

    mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity),

    case mnesia:create_table(project,
                             [{disc_copies, [node()]},
                              {attributes, record_info(fields, project)}]) of
    {aborted, Reason} ->
        case Reason of
        {already_exists, _} ->
            ok;
        _ ->
            lager:alert("create table 'project' failed: ~p", [Reason]),
            erlang:error(create_table_error)
        end;
    {atomic, ok} ->
        ok
    end,

    case mnesia:create_table(task,
                             [{disc_copies, [node()]},
                              {attributes, record_info(fields, task)}]) of
    {aborted, Reason2} ->
        case Reason2 of
        {already_exists, _} ->
            ok;
        _ ->
            lager:alert("create table 'task' failed: ~p", [Reason2]),
            erlang:error(create_table_error)
        end;
    {atomic, ok} ->
        ok
    end,

    case mnesia:create_table(unfetched_subtask,
                             [{disc_copies, [node()]},
                              {record_name, subtask},
                              {attributes, record_info(fields, subtask)}]) of
    {aborted, Reason3} ->
        case Reason3 of
        {already_exists, _} ->
            ok;
        _ ->
            lager:alert("create table 'unfetched_subtask' failed: ~p",
                        [Reason3]),
            erlang:error(create_table_error)
        end;
    {atomic, ok} ->
        ok
    end,

    case mnesia:create_table(fetched_subtask,
                             [{disc_copies, [node()]},
                              {record_name, subtask},
                              {attributes, record_info(fields, subtask)}]) of
    {aborted, Reason4} ->
        case Reason4 of
        {already_exists, _} ->
            ok;
        _ ->
            lager:alert("create table 'fetched_subtask' failed: ~p", [Reason4]),
            erlang:error(create_table_error)
        end;
    {atomic, ok} ->
        ok
    end,

    case mnesia:create_table(finished_subtask,
                             [{disc_copies, [node()]},
                              {record_name, subtask},
                              {attributes, record_info(fields, subtask)}]) of
    {aborted, Reason5} ->
        case Reason5 of
        {already_exists, _} ->
            ok;
        _ ->
            lager:alert("create table 'finished_subtask' failed: ~p",
                        [Reason5]),
            erlang:error(create_table_error)
        end;
    {atomic, ok} ->
        ok
    end,

    case mnesia:create_table(task_count,
                             [{disc_copies, [node()]},
                              {attributes, record_info(fields, task_count)}]) of
    {aborted, Reason6} ->
        case Reason6 of
        {already_exists, _} ->
            ok;
        _ ->
            lager:alert("create table 'task_count' failed: ~p", [Reason6]),
            erlang:error(create_table_error)
        end;
    {atomic, ok} ->
        ok
    end,

    ok.
