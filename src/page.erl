-module(page).
-export([eval/2, split/1, sort/2]).

%% @doc checks if the page contains the query.
eval(_Query, _PageData) ->
% use re module, to regex through the page data
% returns a tuple a number of hits and containing all hyperlinks
    {0, []}.


split(Page) -> split(Page, []).

split([], Senetences) -> Sentences;
split(Page, Sentences) ->
    {Sentence, Rest} = lists:split(".", [Page]),
    split(Rest, [Sentence | Sentences]).

sort(Sentences, Query) -> 
    
    RgRs = fun(Sentence) -> regex_result(Sentence, Query) end,
    RegexResults = lists:map(RgRs, Sentences),

    MatchResult = lists:map(fun match_result, RegexResults).

regex_result(Sentences, Query) ->
    RegexResult = re:run(Sentences, Query).

match_result(RegexResult) ->
    case RegexResult of
	{match, SortedItem} ->
		SortedItem;
	nomatch ->
	    []
    end.

