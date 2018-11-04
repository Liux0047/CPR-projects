%% @author Xiao Liu <liux0047@gmail.com>
%% This module serves as a listener for incoming requests

-module(docking_listener).
-export([start_link/0, init/0]).

%% @doc Starts the process and links to its caller
-spec start_link() -> {ok, pid()}.
start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

%% @doc Initialize the listener.
init() ->
    listen().

%% @doc Listens for incoming message
listen() ->
    receive
        {release_moped, Name, From} ->
            From ! {reply, docking:release_moped(Name)},
            listen();
        {secure_moped, Name, From} ->
            From ! {reply, docking:secure_moped(Name)},
            listen()
    end.