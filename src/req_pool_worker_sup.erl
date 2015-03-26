%% @author dkelsey
%% @doc @todo Add description to req_pool_worker_sup.


-module(req_pool_worker_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).


start_link(MFA = {_,_,_}) ->
	supervisor:start_link(?MODULE, MFA).

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
%init([]) ->
%    AChild = {'AName',{'AModule',start_link,[]},
%	      permanent,2000,worker,['AModule']},
%    {ok,{{one_for_all,0,1}, [AChild]}}.
init({M,F,A}) ->
	MaxRestart = 5,
	MaxTime = 3600,
	inets:start(), % start this here and not in the workers themselves.
	timer:start(), % start this here too.
	ets:new(cache, [set, public, named_table]),
	StopwordsFile = application:get_env(req_pool, stopwords_file, "./stopwords"),
%	io:format("~p~n", [StopwordsFile]),
	{ok, StopWords} = file:read_file(StopwordsFile),
	ets:new(stopwords, [set, named_table]),
	StopTokens = string:tokens( binary_to_list(StopWords), "\n"),
	StopWordsList = [{T,0} || T <- StopTokens],
	ets:insert(stopwords, StopWordsList),
	{ok, {{simple_one_for_one, MaxRestart, MaxTime},
			[{ppool_worker,
			  {M,F,A},
			  temporary, 5000, worker, [M]}]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


