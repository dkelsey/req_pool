%% @author dkelsey
%% @doc @todo Add description to req_pool_api.


-module(req_pool_api).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, stop/0, start_pool/3,
		 run/2, sync_queue/2, async_queue/2, stop_pool/1]).
 
start_link() ->
	req_pool_supersup:start_link().
 
stop() ->
	req_pool_supersup:stop().
 
start_pool(Name, Limit, {M,F,A}) ->
	req_pool_supersup:start_pool(Name, Limit, {M,F,A}).
 
stop_pool(Name) ->
	req_pool_supersup:stop_pool(Name).
 
run(Name, Args) ->
	req_pool:run(Name, Args).
 
async_queue(Name, Args) ->
	req_pool:async_queue(Name, Args).
 
sync_queue(Name, Args) ->
	req_pool:sync_queue(Name, Args).

%% ====================================================================
%% Internal functions
%% ====================================================================


