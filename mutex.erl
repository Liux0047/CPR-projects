-module(mutex).
-export([
    start/0,
    free/0,
    wait/0,
    signal/0,
    start_proc/2,
    acquire/1
]).

start() ->
    %exit(whereis(mutex), ok),
    register(mutex, spawn(mutex, free, [])),
    start_proc(100, 200),
    start_proc(40, 400).

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
            %io:format("Received signal", []),
            Pid ! ok,
            free()
    end.

free() ->
    receive
        { wait, Pid } -> 
            %io:format("Received wait", []),
            Pid ! ok,
            busy()
    end.


