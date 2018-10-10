-module(my_db).
-export([
    start/0,
    loop/1,
    read/1,
    write/2
]).

start() ->
    register(my_db, spawn(my_db, loop, [db:new()])).

write(Key, Element) ->
    my_db ! {write, self(), {Key, Element}},
    receive 
        ok -> io:format("ok in write")
    end.


read(Key) ->
    my_db ! {read, self(), Key},
    receive
        {ok, Element} -> 
            io:format("Ok got: ~p~n", [Element])
    end.

loop(Records) ->
    receive
        {write, Pid, {Key, Value}} ->
            Pid ! ok,
            loop(db:write(Key, Value, Records));
        {read, Pid, Key} ->
            Pid ! db:read(Key, Records),
            loop(Records)
    end.
