-module(spider).
-compile(export_all).

init(Page, Query, Lifetime, Pid) ->
    spawn(?MODULE, search, [Page, Query, Lifetime, Pid]).

get_page(_URL) ->
    "<!DOCTYPE html><html><head></head><body></body></html>".

search(_URL, _Query, 0, _Pid) -> 
    do_nothing;
search(URL, Query, Lifetime, Pid) ->
    PageData = get_page(URL),
    {Hits, Hyperlinks} = page:eval(Query, PageData),
    if Hits =/= 0 -> Pid ! {page, URL} end,

    SpawnSpider = 
        fun (Link) -> init(Link, Query, Lifetime - 1, Pid) end,

    % spawn a new spider for each link found
    lists:foreach(SpawnSpider, Hyperlinks),
    % spawns a new spider search on each new hyperlink
    ok.

check_links([], RelevantURLs)   -> RelevantURLs;
check_links(URLs, RelevantURLs) -> ok.

%% @doc stores the sites visited so that a page isn't visited twice.
link_repo([]) -> ok;
link_repo(_Data) ->
    receive
        {find, _URL} -> ok;
        {store, _URL} -> ok
    end.

