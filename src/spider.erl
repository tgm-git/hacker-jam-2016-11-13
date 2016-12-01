-module(spider).
-compile(export_all).
-include("spider-context.hrl").

%% added for convenience so it can be called without making the struct
init(Page, Lifetime, Query, Listener, Repo) ->
    Context = #context{query = Query, listener = Listener, repo = Repo},
    init(Page, Lifetime, Context).

init(Page, Lifetime, Context) when is_record(Context, context) ->
    spawn(?MODULE, search, [Page, Lifetime, Context]).

%% @TODO Implement this
get_page(_URL) ->
    "<!DOCTYPE html><html><head></head><body></body></html>".

search(_URL, 0, _Con) -> die;
search(URL, Life, Con) when is_record(Con, context) ->
    PageData = get_page(URL),
    {Hits, Hyperlinks} = page:eval(Con#context.query, PageData),
    if Hits =/= 0 -> Con#context.listener ! {page, URL} end,

    SpawnSpider = 
        fun (Link) -> init(Link, Life - 1, Con) end,

    %% filters out all the links that have already been visited
    FilteredLinks = check_links(Hyperlinks, Con), %% @TODO add timeout check

    %% spawn a new spider for each link found
    lists:foreach(SpawnSpider, FilteredLinks),

    %% add the new the links to the repo
    store_links(FilteredLinks, Con),
    ok.

check_links(Urls, Context) -> check_links(Urls, [], Context).

%% @doc sends a message to the link repository and checks if it exists
%% if it doesn't, it'll be added to Relevants, if does it won't.
check_links([], Relevants, _Context)  -> Relevants;
check_links(URLs, Relevants, Context) -> 
    [URLHead | URLTail] = URLs,
    Self = self(),
    %% make_ref is used to ensure the message is sent back
    %% to the correct receiver by matching on the Ref
    Ref = make_ref(),
    Context#context.repo ! {Self, {find, URLHead, Ref}},
    receive
        {hit,  Ref} -> check_links(URLTail, Relevants);
        {miss, Ref} -> check_links(URLTail, [URLHead | Relevants])
    after 2000 -> timeout
    end.

%% @doc goes through a list of links, sending them to the repo.
store_links(Links, C) when is_record(C, context) ->
    Callback = 
        fun(Link) -> C#context.repo ! {store, Link} end,
    lists:foreach(Callback, Links),
    ok.
