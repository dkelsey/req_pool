%% @author dkelsey
%% @doc @todo Add description to req_pool_supersup.


-module(req_pool_supersup).
-behaviour(supervisor).
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
start_link() ->
	supervisor:start_link({local, ppool}, ?MODULE, []).

%% technically, a supervisor can not be killed in an easy way.
%% Let's do it brutally!
stop() ->
	case whereis(ppool) of
		P when is_pid(P) ->
			exit(P, kill);
		_ -> ok
	end.

start_pool(Name, Limit, MFA) ->
	ChildSpec = {Name,
				  {req_pool_sup, start_link, [Name, Limit, MFA]},
				   permanent, 10500, supervisor, [req_pool_sup]},
	supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
	supervisor:terminate_child(ppool, Name),
	supervisor:delete_child(ppool, Name).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
	MaxRestart = 6,
	MaxTime = 3600,
	{ok, {{one_for_one, MaxRestart, MaxTime}, []}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


