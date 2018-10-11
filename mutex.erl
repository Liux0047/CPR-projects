-module(mutex).
-export([
    start/0,
    init_mutex/0,
    free/0,
    wait/0,
    signal/0,
    start_proc/2,
    acquire/1
]).

start() ->
    register(mutex, spawn(mutex, init_mutex, [])).

init_mutex() ->
    process_flag(trap_exit, true),
    register(pa, start_proc(1000, 4000)),
    register(pb, start_proc(1000, 4000)),
    free().

start_proc(Timeout, Duration) ->
    timer:sleep(Timeout),
    spawn(mutex, acquire, [Duration]).

acquire(Duration) ->
    io:format("Process ~p trying to acquire lock~n", [self()]),
    wait(),
    timer:sleep(Duration),
    signal().

wait() ->
    mutex ! {wait, self()},
    receive
        ok -> io:format("Process ~p Got lock~n", [self()])
    end.

signal() ->
    mutex ! {signal, self()},
    receive
        ok -> io:format("Process ~p release lock~n", [self()])
    end.


busy() ->
    receive
        {signal, Pid} ->
            Pid ! ok, 
            unlink(Pid),
            free();
        {'EXIT', Pid, Reason} ->
            io:format("Received EXIT in busy() from ~p, reason: ~s~n", [Pid, Reason]),
            unlink(Pid),
            free()
    end.

free() ->
    receive
        { wait, Pid } -> 
            Pid ! ok,
            link(Pid),
            busy()
    end.


