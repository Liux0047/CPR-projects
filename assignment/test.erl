-module(test).
-export([part1/0]).

part1() ->
    io:format("~w~n", [docking:start_link(3,0, kellogg)]),
    io:format("~w~n", [docking:release_moped(kellogg)]),
    io:format("~w~n", [docking:secure_moped(kellogg)]).