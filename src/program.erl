-module(program).
-compile(export_all).

start() ->
    ok.

run_display(Page, Query) -> 
    Sentences = page:split(Page),
    SortedSentences = page:sort(Sentences, Query),

    lists:foreach(fun io:format, SortedSentences).

