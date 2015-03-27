%% @author dkelsey
%% @doc @todo Add description to req_worker.


-module(req_worker).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%-import(statsd).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_link/2]).
-export([shutdown/0]).
-export([get_uri/1]).

start_link(Uri, SendTo) ->
% pass the Uri and Pid of the requester onto init for processing
%	gen_server:start_link({local, ?MODULE}, ?MODULE, [Uri, SendTo], [] ).
% the method above names the process which causes clashes when starting multiple workers
	gen_server:start_link(?MODULE, [Uri, SendTo], [] ).
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], [] ).
shutdown() ->
	gen_server:call(?MODULE, stop).
get_uri(Uri) ->
	gen_server:call(?MODULE, {get_uri, Uri}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { token_string=" ",
			     limit=10,
				 search_path="//item/description/text()"
}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	inets:start(),
    {ok, #state{}};
init([Uri, SendTo]) ->
	% inets:start(), % now started in the req_worker_sup
	% initialize and send a message to self() to handle the request
	% ets:new(cache, [set, named_table]),  % create the ets table in the req_pool_worker_sup only once.
	TokenString   = application:get_env(req_pool, token_string,   " "),
	Limit         = application:get_env(req_pool, count_limit,    10),
	SearchPath    = application:get_env(req_pool, search_path,    "//item/description/text()"),
%	StopwordsFile = application:get_env(req_pool, stopwords_file, "./stopwords"),
%	io:format("~p~n", [StopwordsFile]),
%	{ok, StopWords} = file:read_file(StopwordsFile),
%	ets:new(stopwords, [set, named_table]),
%	StopTokens = string:tokens( binary_to_list(StopWords), "\n"),
%	StopWordsList = [{T,0} || T <- StopTokens],
%	ets:insert(stopwords, StopWordsList),
	self() ! {process_uri, Uri, SendTo},
	{ok, #state{token_string=TokenString, limit=Limit, search_path=SearchPath}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
%handle_call({get_uri, Uri}, From, State) ->
%	case do_check_cache(Uri) of
%		{absent} ->
%			{ok, DecodedUri} = do_decode_uri(Uri),
%			gen_server:cast(?MODULE, {get_uri, Uri, DecodedUri, From});
%		{ok, {_CachedURI, JSONString}} ->
%			gen_server:cast(?MODULE, {reply,  JSONString, From})
%	end,
%	{reply, ok, State};
%handle_call({get_uri, Uri, SendTo}, _From, State) ->
%	{ok, Headers} = do_get_uri(Uri),
%	io:format("here now"),
%	SendTo ! {ok, Headers},
%	{stop, normal, ok, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
	{reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
%
handle_cast({decode_uri, Uri, SendTo}, State) ->
    {T, {ok, DecodedUri}} = timer:tc(rss_wc_lib, decode_uri, [Uri]),
	statsd:timing("decode_uri", T),
	gen_server:cast(self(), {get_uri, Uri, DecodedUri, SendTo}),
	{noreply, State};
handle_cast({get_uri, Uri, DecodedUri, SendTo}, State) ->
	{T, {ok, XMLBody}} = timer:tc(rss_wc_lib, get_uri, [DecodedUri]),
	statsd:timing("get_uri", T),
	statsd:count("get_uri", erlang:length(XMLBody)),
	gen_server:cast(self(), {parse_xml, Uri, XMLBody, SendTo}),
	{noreply, State};
handle_cast({parse_xml, Uri, XMLBody, From}, State) ->
	{T, {ok, Text}} = timer:tc(rss_wc_lib, parse_xml, [XMLBody, State#state.search_path]),
	statsd:timing("parse_xml", T),
	statsd:count("parse_xml", erlang:length(Text)),
	gen_server:cast(self(), {parse_text, Uri, Text, From}),
	{noreply, State};
handle_cast({parse_text, Uri, Text, From}, State) ->
	{T, {ok, FilteredText}} = timer:tc(rss_wc_lib, parse_text, [Text]),
	statsd:timing("parse_text", T),
	statsd:count("parse_text", erlang:length(FilteredText)),
	gen_server:cast(self(), {tokenize_text, Uri, FilteredText, From}),
	{noreply, State};
handle_cast({tokenize_text, Uri, Text, From}, State) ->
	{T, {ok, FilteredText}} = timer:tc(rss_wc_lib, tokenize_text, [Text, State#state.token_string]),
	statsd:timing("tokenize_text", T),
	statsd:count("tokenize_text", erlang:length(FilteredText)),
	gen_server:cast(self(), {filter_stopwords, Uri, FilteredText, From}),
	{noreply, State};
handle_cast({filter_stopwords, Uri, Text, From}, State) ->
	{T, {ok, FilteredTokens, StopwordCounts}} = timer:tc(rss_wc_lib, filter_stopwords, [Text]),
	statsd:timing("filter_stopwords", T),
	statsd:count("filter_stopwords.filteredTokens", erlang:length(FilteredTokens)),
	statsd:count("filter_stopwords.StopwordCount", erlang:length(StopwordCounts)),
	gen_server:cast(self(), {count_tokens, Uri, FilteredTokens, StopwordCounts, From}),
	{noreply, State};
handle_cast({count_tokens, Uri, FilteredTokens, StopwordCounts, From}, State) ->
	{T, {ok, CountedTokens}} = timer:tc(rss_wc_lib, count_tokens, [FilteredTokens]),
	statsd:timing("count_tokens", T),
	statsd:count("count_tokens", erlang:length(CountedTokens)),
	gen_server:cast(self(), {sort_tokens, Uri, CountedTokens, StopwordCounts, From}),
	{noreply, State};
handle_cast({sort_tokens, Uri, CountedTokens, StopwordCounts, From}, State) ->
	{T, {ok, SortedTokens}} = timer:tc(rss_wc_lib, sort_tokens, [CountedTokens]),
	statsd:timing("sort_tokens", T),
	statsd:count("sort_tokens", erlang:length(SortedTokens)),
	gen_server:cast(self(), {limit_tokens, Uri, SortedTokens, StopwordCounts, From}),
	{noreply, State};
handle_cast({limit_tokens, Uri, Tokens, StopwordCounts, From}, State) ->
	{T, {ok, LimitedTokens}} = timer:tc(rss_wc_lib, limit_tokens, [Tokens, State#state.limit]),
	statsd:timing("limit_tokens", T),
	statsd:count("limit_tokens", erlang:length(LimitedTokens)),
	gen_server:cast(self(), {format_to_json, Uri, LimitedTokens, StopwordCounts, From}),
	{noreply, State};
handle_cast({format_to_json, Uri, Tokens, StopwordCounts, From}, State) ->
	{T, {ok, JSON_String}} = timer:tc(rss_wc_lib, format_to_json, [Tokens, StopwordCounts]),
	statsd:timing("format_to_json", T),
	statsd:count("format_to_json", erlang:length(erlang:binary_to_list(JSON_String))),
	gen_server:cast(self(), {cache, Uri, JSON_String, From}),
	{noreply, State};
handle_cast({cache, Uri, JSON_String, From}, State) ->
	{T, {ok, cached}} = timer:tc(rss_wc_lib, cache, [Uri, JSON_String]),
	statsd:timing("cache", T),
	gen_server:cast(self(), {reply, JSON_String, From}),
	{noreply, State};
handle_cast({reply, JSON_String, SendTo}, State) ->
	SendTo ! {ok, JSON_String},
	{stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({process_uri, Uri, SendTo}, State) ->
% cast get_uri message to handle the get...perhaps unnecessary
% The processing could be done here, saving a level of indirection
%	SendTo ! {ok, "goober"},
%	{stop, normal,  State};
%    io:format("handle_info... calling self(): ~p {get_uri, Uri, SendTo}~n", [self()]),
	gen_server:cast(self(), {decode_uri, Uri, SendTo}),
	{noreply, State};
handle_info(_Info, State) ->
%	io:format("~p~n", [Info]),
    {noreply, State}.

%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
%	io:format("~p is terminating~n", [?MODULE]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================