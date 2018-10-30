-module(station_recoverer).
-export([start_link/0, init/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    recover_station(docking_server:get_all_stations()).

recover_station([]) ->
    ok;
recover_station([{Name, Total, Occupied} | T ]) ->
    station_supervisor:start_child(Total, Occupied, Name),
    recover_station(T).