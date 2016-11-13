-module(program).
-compile(export_all).

start(Page, Query, Lifetime) ->
    ListenerPid = spawn(listener, init, []),
    spider:init(Page, Query, Lifetime, ListenerPid).

run_display(Page, Query) -> 
    Sentences = page:split(Page),
    SortedSentences = page:sort(Sentences, Query),

    lists:foreach(fun io:format/2, SortedSentences).
