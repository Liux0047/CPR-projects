-module(docking_listener).
-export([start_link/0, init/0, listen/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

init() ->
    listen().

listen() ->
    receive
        {release_moped, Name, From} ->
            From ! docking:release_moped(Name),
            listen();
        {secure_moped, Name, From} ->
            From ! docking:secure_moped(Name),
            listen()
    end.