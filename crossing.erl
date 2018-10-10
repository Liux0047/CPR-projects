-module(crossing).
-export([
    init/2,
    start/2,
    work/2
]).

init(N, M) -> 
    spawn(crossing, start, [N, M]),
    ok.


start(N, M) ->
    Left = spawn(crossing, work, [N div 2, self()]),
    Right = spawn(crossing, work, [N - N div 2 - 1, self()]),
    receive
        ready -> 
            Left ! {halfway, "Hello", M},
            receive_msg(Left, leader),
            receive_msg(Right, leader)
    end.


work(1, FirstPid) ->
    FirstPid ! ready,
    receive_msg(FirstPid, false);
work(N, FirstPid) -> 
    Next = spawn(crossing, work, [N - 1, FirstPid]),
    receive_msg(Next, false).

receive_msg(NextPid, leader) ->
    receive
        {halfway , Msg, 0} ->
            NextPid ! {fullcycle, Msg, 0},
            terminate();
        {halfway, Msg, M} ->
            NextPid ! {fullcycle, Msg, M},
            io:format("Process ~p: ~s ~p|| Halfway~n", [self(), Msg, M]),
            receive_msg(NextPid, leader);
        {fullcycle, Msg, M} ->
            NextPid ! {halfway, Msg, M -1},
            io:format("Process ~p: ~s ~p|| ~n", [self(), Msg, M-1]),
            receive_msg(NextPid, leader)
    after 50000 -> terminate()
    end;
receive_msg(NextPid, false) ->
    receive
        {Way , Msg, 0} ->
            NextPid ! {Way, Msg, 0},
            terminate();
        {Way, Msg, M} ->
            NextPid ! {Way, Msg, M},
            io:format("Process ~p: ~s ~p||~n", [self(), Msg, M]),
            receive_msg(NextPid, false)
    after 50000 -> terminate()
    end.

terminate() ->
    io:format("Process ~p terminating~n", [self()]),
    ok.