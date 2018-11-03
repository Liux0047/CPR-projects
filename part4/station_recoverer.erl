%% @author Xiao Liu <liux0047@gmail.com>
%% @doc This module will recover already started stations upon calling start_link.

-module(station_recoverer).
-export([start_link/0, init/0]).

%% @doc Starts the recovery process and links to the caller
-spec start_link() -> {ok, pid()}.
start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

%% @doc Init the process and kick-off recovery process.
%% Recovers the state of already started stations based on state from docking_server
-spec init() -> ok.
init() ->
    recover_station(docking_server:get_all_stations()).

recover_station([]) ->
    ok;
recover_station([{Name, Total, Occupied} | T ]) ->
    station_supervisor:start_child(Total, Occupied, Name),
    recover_station(T).