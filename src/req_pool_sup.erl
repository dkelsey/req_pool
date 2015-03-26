-module(req_pool_sup).
-behaviour(supervisor).

%-export([start_link/0]).
-export([start_link/3]).
-export([init/1]).

% start_link() was commented out
%start_link() ->
%	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_link(Name, Limit, MFA) ->
	supervisor:start_link(?MODULE, {Name, Limit, MFA}).

init({Name, Limit, MFA}) ->
	MaxRestart = 1,
	MaxTime = 3600,
	{ok, {{one_for_all, MaxRestart, MaxTime},
	%		[{serv,
			[{req_pool,
				{req_pool, start_link, [Name, Limit, self(), MFA]},
				permanent,
				5000, % Shutdown time
				worker,
				[req_pool]}]}}.