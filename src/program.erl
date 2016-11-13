-module(program).
-compile(export_all).

start(Page, Query, Lifetime) ->
    ListenerPid = spawn(listener, init, []),
    spider:init(Page, Query, Lifetime, ListenerPid).
