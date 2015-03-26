-module(req_pool_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
%	req_pool_sup:start_link().
	req_pool_supersup:start_link().

stop(_State) ->
	ok.
