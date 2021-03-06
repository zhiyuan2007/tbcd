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
-module(default_handler).
-behaviour(cowboy_http_handler).
-author('flygoast@126.com').


-export([init/3,
         handle/2,
         terminate/3]).


-record(state, {}).


init(_, Req, _Opts) ->
    {ok, Req, #state{}}.


handle(Req, State=#state{}) ->
    case tbcd_validation:valid_request(Req) of
    {yes, _Body, Req2} ->
        handle_request(Req2, State);
    {no, _, Req2} ->
        tbcd_reply:reply_plain(Req2, State, 403, <<"access forbidden">>)
    end.


handle_request(Req, State) ->
    {Path, Req2} = cowboy_req:path(Req),
    lager:info("[default]: invalid path: ~p", [Path]),
    tbcd_reply:reply_plain(Req2, State, 200, <<"invalid path">>).


terminate(_Reason, _Req, _State) ->
    ok.
