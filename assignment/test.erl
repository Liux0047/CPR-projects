-module(test).
-export([part1/0]).

part1() ->
    io:format("~w~n", [docking:start_link(3,1, kellogg)]),
    io:format("~w~n", [docking:release_moped(kellogg)]),
    io:format("~w~n", [docking:release_moped(kellogg)]),
    io:format("~w~n", [docking:secure_moped(kellogg)]),
    io:format("~w~n", [docking:secure_moped(kellogg)]),
    io:format("~w~n", [docking:secure_moped(kellogg)]),
    io:format("~w~n", [docking:secure_moped(kellogg)]).