-module(repo).
-compile(export_all).
-behaviour(gen_server).

%% REPO FUNCTIONS
start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

create_url(Pid, URL) ->
    gen_server:cast(Pid, {create, URL}).

read_url(Pid, URL) ->
    gen_server:call(Pid, {read, URL}).

delete_url(Pid, URL) ->
    gen_server:cast(Pid, {delete, URL}).

drop(Pid) ->
    gen_server:cast(Pid, drop_repo).

%% GEN_SERVER CALLBACK FUNCTIONS
init([]) -> {ok, []}.

handle_info(_Info, _State) -> {noreply, _State}.

% handle_cast(_Request, _State) -> {noreply, _State}.
handle_cast({create, URL}, Links) ->
    {noreply, [URL | Links]};
handle_cast({delete, URL}, Links) ->
    {noreply, [Link || Link <- Links, Link =/= URL]};
handle_cast(drop_repo, _Links) ->
    {noreply, []}.


handle_call({read, URL}, _From, Links) -> 
    case lists:member(URL, Links) of
        true -> {reply, hit};
        false -> {reply, miss}
    end.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

terminate(normal, _State) -> ok;
terminate(save, State) -> 
    FileReadyState = lists:join("\n", State) ++ ["\n"],
    case file:write_file("repository-dump.txt", FileReadyState) of
        ok -> ok;
        {error, badarg} ->
            io:format("Couldn't dump the repository, bad format.~n"),
            ok;                                 % Maybe change to throwing an error
        {error, eacces} ->
            io:format("Didn't have write-access to repository-dump.txt.~n"),
            ok                                  % Same goes for this
    end.
                               
    
