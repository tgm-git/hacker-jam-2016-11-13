-module(program).
-compile(export_all).

start() ->
    ok.

split_page(Page) -> split_page(Page, []).

split_page([], Senetences) -> Sentences;
split_page(Page, Sentences) ->
    {Sentence, Rest} = lists:split(".", [Page]),
    split_page(Rest, [Sentence | Sentences]).

sort_page(Sentences, Query) -> 
    
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

run_display(Page, Query) -> 
    Sentences = split_page(Page),
    SortedSentences = sort_page(Sentences, Query),

    lists:foreach(fun io:format, SortedSentences).



