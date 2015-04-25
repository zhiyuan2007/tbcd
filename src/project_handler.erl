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

-module(project_handler).
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
    case tbcd_validation:valid_request(Req) of
    {yes, Body,  Req2} ->
        handle_request(Req2, Body, State);
    {no, _, Req2} ->
        tbcd_reply:reply_plain(Req2, State, 403, <<"access forbidden">>)
    end.


handle_request(Req, Body, State) ->
    case catch jiffy:decode(Body) of
    {Ls} ->
        case lists:keyfind(<<"mode">>, 1, Ls) of
        false ->
            lager:info("mode argument not supplied", []),
            Content = jiffy:encode({[{<<"code">>, 1},
                                     {<<"reason">>,
                                      <<"mode argument not supplied">>}
                                    ]}),
            tbcd_reply:reply_json(Req, State, 200, Content);
        {_, <<"add">>} ->
            case project_add(Ls) of
            {aborted, _Reason} ->
                tbcd_reply:reply_plain(Req, State, 500,
                                       <<"Internal server error">>);
            {error, Reason} ->
                Content = jiffy:encode({[{<<"code">>, 1},
                                         {<<"reason">>,
                                          list_to_binary(Reason)}
                                        ]}),
                tbcd_reply:reply_json(Req, State, 200, Content);
            ok ->
                Content = jiffy:encode({[{<<"code">>, 0}]}),
                tbcd_reply:reply_json(Req, State, 200, Content)
            end;
        {_, <<"delete">>} ->
            case project_delete(Ls) of
            {aborted, _Reason} ->
                tbcd_reply:reply_plain(Req, State, 500,
                                       <<"Internal server error">>);
            {error, Reason} ->
                Content = jiffy:encode({[{<<"code">>, 1},
                                         {<<"reason">>,
                                          list_to_binary(Reason)}]}),
                tbcd_reply:reply_json(Req, State, 200, Content);
            ok ->
                Content = jiffy:encode({[{<<"code">>, 0}]}),
                tbcd_reply:reply_json(Req, State, 200, Content)
            end;
        {_, <<"select">>} ->
            case lists:keyfind(<<"project">>, 1, Ls) of
            false ->
                Projects = project_select(),
                lager:info("get all projects: ~p", [Projects]),
                Content = jiffy:encode({[{<<"code">>, 0},
                                         {<<"projects">>, Projects}]}),
                tbcd_reply:reply_json(Req, State, 200, Content);
            {_, <<"">>} ->
                {error, "project argument is empty"};
            {_, P} when is_binary(P) ->
                case project_select(P) of
                {aborted, _Reason} ->
                    tbcd_reply:reply_plain(Req, State, 500,
                                           <<"Internal server error">>);
                {error, Reason} ->
                    Content = jiffy:encode({[{<<"code">>, 1},
                                             {<<"reason">>,
                                              list_to_binary(Reason)}]}),
                    tbcd_reply:reply_json(Req, State, 200, Content);
                Rs ->
                    Content = jiffy:encode({[{<<"code">>, 0},
                                             {<<"workers">>, Rs}]}),
                    tbcd_reply:reply_json(Req, State, 200, Content)
                end;
            _ ->
                {error, "project argument is not string"}
            end;
        {_, Mode} ->
            lager:error("invalid mode argument: ~p", [Mode]),
            Content = jiffy:encode({[{<<"code">>, 1},
                                     {<<"reason">>,
                                      <<"invalid mode argument">>}]}),
            tbcd_reply:reply_json(Req, State, 200, Content)
        end;
    {error, Reason} ->
        lager:info("invalid json: ~p", [Reason]),
        Content = jiffy:encode({[{<<"code">>, 1},
                                 {<<"reason">>, <<"invalid json">>}]}),
        tbcd_reply:reply_json(Req, State, 200, Content)
    end.


terminate(_Reason, _Req, _State) ->
    ok.


project_select() ->
    mnesia:dirty_all_keys(project).


project_select(P) ->
    F = fun() ->
            case mnesia:read(project, P) of
            [] ->
                {error, "project not existed"};
            [#project{workers = W}] ->
                Workers = sets:to_list(W),
                Workers
            end
        end,

    case mnesia:transaction(F) of
    {atomic, Rs} ->
        lager:info("select: project: ~p, result: ~p", [P, Rs]),
        Rs;
    {aborted, Reason} ->
        lager:error("select: mnesia error: ~p", [Reason]),
        {aborted, Reason}
    end.


project_delete(Ls) ->
    case lists:keyfind(<<"workers">>, 1, Ls) of
    false ->
        project_delete(Ls, []);
    {_, W} when is_list(W) ->
        project_delete(Ls, W);
    _ ->
        {error, "workers argument is not array"}
    end.

project_delete(Ls, Workers) ->
    WkSet = sets:from_list(Workers),

    case lists:keyfind(<<"project">>, 1, Ls) of
    false ->
        {error, "project argument not supplied"};
    {_, <<"">>} ->
        {error, "project argument is empty"};
    {_, P} when is_binary(P) ->
        F = fun() ->
                case mnesia:wread({project, P}) of
                [] ->
                    {error, "project not existed"};
                [#project{workers = OW}] ->
                    case Workers of
                    [] -> %% delete the project
                        mnesia:delete({project, P}),
                        ok;
                    _ -> %% delete the workers from project
                        NewWorkers = sets:subtract(OW, WkSet),
                        mnesia:write(#project{name = P, workers = NewWorkers}),
                        ok
                    end
                end
            end,

        case mnesia:transaction(F) of
        {atomic, Rs} ->
            lager:info("delete: project: ~p, result: ~p", [P, Rs]),
            Rs;
        {aborted, Reason} ->
            lager:error("delete: mnesia error: ~p", [Reason]),
            {aborted, Reason}
        end;
    _ ->
        {error, "project argument is not string"}
    end.


project_add(Ls) ->
    case lists:keyfind(<<"workers">>, 1, Ls) of
    false ->
        project_add(Ls, []);
    {_, W} when is_list(W) ->
        project_add(Ls, W);
    _ ->
        {error, "workers argument is not array"}
    end.


project_add(Ls, Wk) ->
    %% drop the duplicated
    WkSet = sets:from_list(Wk),

    case lists:keyfind(<<"project">>, 1, Ls) of
    false ->
        {error, "project argument not supplied"};
    {_, <<"">>} ->
        {error, "project argument is empty"};
    {_, P} when is_binary(P) ->
        F = fun() ->
                case mnesia:wread({project, P}) of
                [] ->
                    mnesia:write(#project{name = P, workers = WkSet}),
                    ok;
                [#project{workers = OW}] ->
                    NewWorkers = sets:union(OW, WkSet),
                    mnesia:write(#project{name = P, workers = NewWorkers}),
                    ok
                end
            end,

        case mnesia:transaction(F) of
        {atomic, Rs} ->
            lager:info("add workers: project: ~p, result: ~p", [P, Rs]),
            Rs;
        {aborted, Reason} ->
            lager:error("add workers: mnesia error: ~p", [Reason]),
            {aborted, Reason}
        end;
    _ ->
        {error, "project argument is not string"}
    end.
