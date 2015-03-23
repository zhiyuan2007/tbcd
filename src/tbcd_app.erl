-module(tbcd_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	tbcd_sup:start_link().

stop(_State) ->
	ok.
