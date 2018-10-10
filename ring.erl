-module(ring).
-export([
    init/2,
    start/2,
    work/2
]).

init(N, M) -> 
    spawn(ring, start, [N, M]),
    ok.


start(N, M) ->
    Next = spawn(ring, work, [N-1, self()]),
    receive
        ready -> 
            Next ! {passon, "Hello", M},
            receive_msg(Next, false)
    end.


work(1, FirstPid) ->
    FirstPid ! ready,
    receive_msg(FirstPid, true);
work(N, FirstPid) -> 
    Next = spawn(ring, work, [N - 1, FirstPid]),
    receive_msg(Next, false).

receive_msg(NextPid, IsLast) ->
    receive
        {passon , Msg, 0} ->
            NextPid ! {passon, Msg, 0},
            terminate();
        {passon, Msg, M} ->
            NextPid ! {passon, Msg, 
            case IsLast of 
                true -> M -1;
                _ -> M
            end
            },
            io:format("~p: ~s ~p||~n", [self(), Msg, M]),
            receive_msg(NextPid, IsLast)
    after 50000 -> terminate()
    end.

terminate() ->
    ok.