%% @author dkelsey
%% @doc @todo Add description to rss_wc_lib.


-module(rss_wc_lib).

%% ====================================================================
%% API functions
%% ====================================================================
-export([check_cache/1]).
-export([cache/2]).
-export([decode_uri/1]).
-export([get_uri/1]).
-export([parse_xml/2]).
-export([parse_text/1]).
-export([tokenize_text/2]).
-export([filter_stopwords/1]).
-export([count_tokens/1]).
-export([sort_tokens/1]).
-export([limit_tokens/2]).
-export([format_to_json/2]).

-include_lib("xmerl/include/xmerl.hrl").

%-record(state, { token_string=" ",
%			     limit=10,
%				 search_path="//item/description/text()"
%}).

check_cache(Uri) ->
	case ets:lookup(cache, Uri) of
		[] -> {absent};
		[JSONString] -> {ok, JSONString}
	end.
cache(Uri, JSONString) ->
	ets:insert(cache, {Uri, JSONString}),
	{ok, cached}.
decode_uri(Uri) ->
	DecodedUri = http_uri:decode(binary_to_list(Uri)),
	{ok, DecodedUri}.
get_uri(DecodedUri) ->
	{ok, {{_Version, HTTPCode, _ReasonPhrase}, _Headers, XMLBody}} = httpc:request(DecodedUri),
	case HTTPCode of
		200 ->
			{ok, XMLBody};
		301 ->
			{ok, XMLBody};
		302 ->
			{ok, XMLBody}
	end.
parse_xml(XMLBody, SearchPath) ->
	{XML_Body, _RemainingText = "" } = xmerl_scan:string(XMLBody),	
	XML_Items = xmerl_xpath:string(SearchPath, XML_Body),
	Text = lists:concat(lists:map(fun(XmlText) -> #xmlText{value=TextValue} = XmlText, string:to_lower(unicode:characters_to_list(TextValue, utf8)) end, XML_Items)),
	{ok, Text}.
parse_text(Text) ->
	LowerCase = string:to_lower(Text),
	FilteredText = lists:map(fun(C) -> case (C > 255) of true -> 65; _ -> C end end, LowerCase),
	{ok, string:to_lower(FilteredText)}.
tokenize_text(Text, TokenString) ->
%	{ok, TokenString} = application:get_env(?MODULE, token_string),
	Tokens = string:tokens(Text, TokenString),
	{ok, Tokens}.
filter_stopwords(Tokens) ->
%  Copy the stopwords ets set [{"a",0},{"b",0},{"c",0},...]
	StopWords = ets:tab2list(stopwords),
%	NewStopWords = [{K,0} || {K,_} <- StopWords],
	TempTable = ets:new(mystopwords, [set]),
	ets:insert(TempTable, StopWords),
	F = fun(X) -> case ets:lookup(TempTable, X) of [] -> [X]; [{X,V}] -> ets:insert(TempTable, {X,V+1}), [] end end,
	StrippedTokens = lists:flatmap(F, Tokens),
	StopwordCounts = ets:tab2list(TempTable),
	ets:delete(TempTable), 
	{ok, StrippedTokens, StopwordCounts}.
count_tokens(Tokens) ->
	TempTable = ets:new(group, [set]),
	Munge = fun(X) -> case ets:lookup(TempTable, X) of [] -> ets:insert(TempTable, [{X, 1}]), []; [{X, V}] -> ets:insert(TempTable, [{X, V+1}]), [] end end,
	_ = lists:flatmap(Munge, Tokens),
	CountedTokens  = ets:tab2list(TempTable),
	ets:delete(TempTable),
	{ok, CountedTokens}.
sort_tokens(CountedTokens) ->
% example CountedTokens = [{"src", 75}, {"border",75}, {"href",44}...]
	TempTable = ets:new(groupedsorted, [ordered_set]),
	SortByValFun = fun({Key, Val}) -> case ets:lookup(TempTable, Val) of [] -> ets:insert(TempTable, [{Val, [Key]}]), []; [{Val, Vec}] -> ets:insert(TempTable, [{Val, [Key|Vec]}]), [] end end,
	_ = lists:flatmap(SortByValFun, CountedTokens),
	InverseList = ets:tab2list(TempTable),
	ets:delete(TempTable),
% InverseList should now be [{75, ["src","border"]}, {44, ["href","meow",...]},...]
% InverseList will be sorted by Key.
	SortByKeyFun = fun({A, _Al} ,{B, _Bl}) -> A > B end,
	SortedInverseList = lists:sort(SortByKeyFun,InverseList),
% SortedInverseList is now sorted by Key
% now sort the val's in the list
	SortValuesFun = fun({Key, Val}) -> {Key, lists:sort(Val)} end,
	SortedInverseAndValueList = lists:map(SortValuesFun, SortedInverseList),
	PivotKeys = fun({Key,Vals}) -> lists:map(fun(X) -> {X, Key} end, Vals) end,
	TokenCountSortedLists = lists:map(PivotKeys, SortedInverseAndValueList),
	SortedCounts = lists:append(TokenCountSortedLists),
	{ok, SortedCounts}.
limit_tokens(Counts, CountLimit) ->
	LimitedCounts = lists:sublist(Counts, CountLimit),
	{ok, LimitedCounts}.
format_to_json(SortedTokens, StopwordsCount) ->
%	jiffy:decode(<<"{\"words\":[{\"word\":\"foo\",\"count\":1000},{\"word\":\"bar\",\"count\":5000}],\"stopWordsIgnored\":10000}">>).
%{[{<<"words">>,
%   [{[{<<"word">>,<<"foo">>},{<<"count">>,1000}]},
%    {[{<<"word">>,<<"bar">>},{<<"count">>,5000}]}]},
%  {<<"stopWordsIgnored">>,10000}]}
%
%	SortList = [{"first",5},{"third",4},{"second",4},{"sixth",1},{"fifth",1},{"fourth",1}].
	BlowUp = fun({Wd,Val}) -> [{[{<<"word">>,list_to_binary(Wd)},{<<"count">>,Val}]}] end,
	SortedList_in_Json = lists:flatmap(BlowUp, SortedTokens),
% total stopwords - maybe do this long before?
	StopwordsIgnoredTotal = lists:foldl(fun({_, X}, Sum) -> X + Sum end, 0, StopwordsCount),
	Final_Json={[{<<"words">>,SortedList_in_Json},{<<"stopWordsIgnored">>,StopwordsIgnoredTotal}]},
	JSON_string = jiffy:encode(Final_Json),
	{ok, JSON_string}.

%% ====================================================================
%% Internal functions
%% ====================================================================


