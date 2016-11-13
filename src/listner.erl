-module(listner).
-compile(export_all).

init() ->
    ok.

weed_out(_Data) ->
    ok.

listen() ->
    receive
        {data, _Data} -> ok
    end.
