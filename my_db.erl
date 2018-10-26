-module(my_db).
-export([
    start/0,
    loop/1,
    read/1,
    write/2
]).

start() ->
    register(?MODULE, spawn(?MODULE, loop, [db:new()])).

write(Key, Element) ->
    ?MODULE ! {write, self(), {Key, Element}},
    receive 
        ok -> io:format("ok in write")
    end.


read(Key) ->
    ?MODULE ! {read, self(), Key},
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
