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
-module(tbcd_validation).
-author('flygoast@126.com').


-export([start/0,
         valid_request/1,
         acl_allow/1,
         acl_add/1,
         acl_del/1,
         acl_real_del/1,
         sign_add/2,
         sign_get/1,
         sign_random_add/1]).


-record(acl, {ip = undefined, status = undefined}).

-record(sign, {appid = <<"">> :: binary(),
               secret = <<"">> :: binary()}).


sign_start() ->
    case mnesia:create_table(sign,
                             [{disc_copies, [node()]},
                              {attributes, record_info(fields, sign)}]) of
    {aborted, Reason} ->
        case Reason of
        {already_exists, _} ->
            ok;
        _ ->
            lager:alert("create table 'sign' failed: ~p", [Reason]),
            erlang:error(create_table_error)
        end;
    {atomic, ok} ->
        ok
    end.


acl_start() ->
    case mnesia:create_table(acl,
                             [{disc_copies, [node()]},
                              {attributes, record_info(fields, acl)}]) of
    {aborted, Reason} ->
        case Reason of
        {already_exists, _} ->
            ok;
        _ ->
            lager:alert("create table 'acl' failed: ~p", [Reason]),
            erlang:error(create_table_error)
        end;
    {atomic, ok} ->
        ok
    end,

    ok.


acl_allow(IP) ->
    case mnesia:dirty_read(acl, IP) of
    [] ->
        no;
    [#acl{status = undefined}] ->
        no;
    [#acl{status = used}] ->
        yes
    end.


acl_add(IP) ->
    mnesia:dirty_write(acl, #acl{ip = IP, status = used}).


acl_del(IP) ->
    mnesia:dirty_write(acl, #acl{ip = IP, status = undefined}).


acl_real_del(IP) ->
    mnesia:dirty_delete(acl, IP).


sign_random_add(Appid) when is_list(Appid) ->
    BinaryAppid = list_to_binary(Appid),
    sign_random_add(BinaryAppid);
sign_random_add(Appid) ->
    Secret = uuid:to_string(simple, erlang:md5(uuid:uuid1())),
    sign_add(Appid, list_to_binary(Secret)),
    list_to_binary(Secret).


sign_get(Appid) when is_list(Appid) ->
    sign_get(list_to_binary(Appid));
sign_get(Appid) ->
    case mnesia:dirty_read(sign, Appid) of
    [] ->
        undefined;
    [#sign{secret = Secret}] ->
        Secret
    end.


sign_add(Appid, Secret) ->
    catch mnesia:dirty_write(sign, #sign{appid = Appid, secret = Secret}).


sign_check(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),


    case application:get_env(tbcd, sign_check, off) of
    off ->
        {yes, Body, Req2};
    on ->
        {Appid, Req3} = cowboy_req:qs_val(<<"appid">>, Req2),
        case Appid of
        undefined ->
            lager:info("no appid in arguments"),
            {no, Body, Req3};
        _ ->
            {Sign, Req4} = cowboy_req:qs_val(<<"sign">>, Req3),
            case Sign of
            undefined ->
                lager:info("no sign in arguments"),
                {no, Body, Req4};
            _ ->
                case mnesia:dirty_read(sign, Appid) of
                [] ->
                    lager:info("invalid appid"),
                    {no, Body, Req4};
                [#sign{secret = Secret}] ->
                    case sign_check(Appid, Sign, Secret, Body) of
                    write ->
                        {yes, Body, Req4};
                    wrong ->
                        {no, Body, Req4}
                    end
                end
            end
        end
    end.


sign_check(Appid, Sign, Secret, Body) ->
    %% sign = md5(Appid + Body + Secret)
    Context = erlang:md5_init(),
    Context1 = erlang:md5_update(Context, Appid),
    Context2 = erlang:md5_update(Context1, Body),
    Context3 = erlang:md5_update(Context2, Secret),
    Digest = list_to_binary(uuid:to_string(simple,
                                           erlang:md5_final(Context3))),

    if
    Digest =:= Sign ->
        write;
    true ->
        lager:info("invalid sign: write: ~p, request: ~p", [Sign, Digest]),
        wrong
    end.


start() ->
    acl_start(),
    sign_start().


valid_request(Req) ->
    {Method, Req2} = cowboy_req:method(Req),

    case Method of
    <<"POST">> ->
        case application:get_env(tbcd, acl_check, off) of
        on ->
            {{IP, Port}, Req3} = cowboy_req:peer(Req2),
            case acl_allow(IP) of
            no ->
                lager:info("request forbidden: from ~p:~p", [IP, Port]),
                {no, undefined, Req3};
            yes ->
                sign_check(Req3)
            end;
        off ->
            sign_check(Req2)
        end;
    _ ->
        lager:info("request forbidden: method ~p", [Method]),
        {no, undefined, Req2}
    end.
