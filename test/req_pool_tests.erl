%% @author dkelsey
%% @doc @todo Add description to rss_wc_tests.


-module(req_pool_tests).
-include("global.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
%-export([]).


decode_uri_test() ->
	% rss_wc:decode_uri(<<"http%3A%2F%2Fdigg.com%2Frss%2Ftop.rss">>).
	% rss_wc:decode_uri(<<"http%3A%2F%2Frss.nytimes.com%2Fservices%2Fxml%2Frss%2Fnyt%2FHomePage.xml">>).
	Uri = <<"http%3A%2F%2Frss.nytimes.com%2Fservices%2Fxml%2Frss%2Fnyt%2FHomePage.xml">>,
	{ok, DecodedUri} = rss_wc:do_decode_uri(Uri),
	?assert( erlang:length(DecodedUri) > 0).

% This test need inets to be started.
get_uri_test() ->
	URI = "http://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml",
	inets:start(), % a hack
	{ok, Body} = rss_wc:do_get_uri(URI),
	?assert(erlang:length(Body) > 0).

parse_xml_test() ->
	{ok, XMLBody} = file:read_file("test/data/NYT_HomePage.xml"),
	{ok, Text} = rss_wc:do_parse_xml(binary_to_list(XMLBody), "//item/description/text()"),
%	?debugFmt("XMLBody length: ~b~n", [erlang:length(binary_to_list(XMLBody))]),
%	?debugFmt("Text length: ~b~n", [erlang:length(Text)]),
	?assert(erlang:length(Text) > 5).

parse_text_test() -> 
	{ok, Text} = file:read_file("test/data/TextData.txt"),
	{ok, FilteredText} = rss_wc:do_parse_text(binary_to_list(Text)),
%	?debugFmt("~nText length: ~p", [erlang:length(binary_to_list(Text))]),
%	?debugFmt("~nFilteredText length: ~b", [erlang:length(FilteredText)]),
%	?debugFmt("Text: ~s~n", [Text]),
%	?debugFmt("NewTokens ~s~n", [NewTokens]),
	?assert(erlang:length(FilteredText) > 0),
	?assertEqual(erlang:length(binary_to_list(Text)), erlang:length(FilteredText)).

tokenize_text_test() ->
	{ok, Text} = file:read_file("test/data/ParsedText.txt"),
	{ok, Tokens} = rss_wc:do_tokenize_text(binary_to_list(Text), " ="),
%	?debugFmt("Text length: ~b~n", [erlang:length(binary_to_list(Text))]),
%	?debugFmt("Tokens length: ~b~n", [erlang:length(Tokens)]),
	?assert(erlang:length(Tokens) > 0).

% This needs to be reworked to be testable
filter_stopwords_test() -> ok.
%	rss_wc:init([]),
%	{ok, Tokens} = file:consult("test/data/tokens"),
%	{ok, [FilteredTokens, StopwordsCounts]} = rss_wc:do_filter_stopwords(Tokens),
%	rss_wc:terminate([], []),
%	?debugFmt("Tokens length: ~b~n", [erlang:length(Tokens)]),
%	?debugFmt("NewFilteredTokens length: ~b~n", [erlang:length(FilteredTokens)]),
%	?debugFmt("NewStopwordsCount length: ~b~n", [erlang:length(StopwordsCounts)]),
%			[?assert(erlang:length(FilteredTokens) > 0)].

count_tokens_test() ->
	{ok, Tokens} = file:consult("test/data/FilteredTokens"),
	{ok, CountedTokens } = rss_wc:do_count_tokens(Tokens),
%	?debugFmt("Tokens length: ~b~n", [erlang:length(Tokens)]),
%	?debugFmt("CountedTokens length: ~b~n", [erlang:length(CountedTokens)]),
	%	?debugFmt("~p", [NewCountedTokens]),
	[?assert(erlang:length(CountedTokens) > 0)].

sort_tokens_test() -> 
	{ok, CountedTokens} = file:consult("test/data/CountedTokens"),
	{ok, SortedTokens} = rss_wc:do_sort_tokens(CountedTokens),
	[?assert(erlang:length(CountedTokens) =:= erlang:length(SortedTokens))].

limit_tokens_test() ->
	{ok, SortedTokens} = file:consult("test/data/SortedTokens"),
	Limit = 10,
	{ok, LimitedSortedTokens} = rss_wc:do_limit_tokens(SortedTokens, Limit),
%	?debugFmt("LimitedSortedTokens lenght: ~b", [erlang:length(LimitedSortedTokens)]),
%	?debugFmt("~p", [LimitedSortedTokens]),
	[?assert(Limit =:= erlang:length(LimitedSortedTokens))].

format_to_json_test() -> 
	% this needs sorted results as well as a count of ignored stopwords
    {ok, StopwordsCount} = file:consult("test/data/StopwordsCount"),
	{ok, LimitedSortedTokens} = file:consult("test/data/LimitedSortedTokens"),
    _StopwordsTotal = lists:foldl(fun({_, X}, Sum) -> X + Sum end, 0, StopwordsCount),
	{ok, JSON_String} = rss_wc:do_format_to_json(LimitedSortedTokens, StopwordsCount),
%	?debugFmt("~s", [JSON_String]),
	[?assert(erlang:length(binary_to_list(JSON_String)) > 5)].

%% ====================================================================
%% Internal functions
%% ====================================================================


