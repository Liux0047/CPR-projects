-module(ev_supervisor).
-behaviour(supervisor).
-include_lib("eunit/include/eunit.hrl").

-export([init/1, start_child/3, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    % not using named_table to avoid access from other processes
    DockingStationDbRef = ets:new(docking_stations, [set, public]),
    SupFlags = #{strategy => rest_for_one,   % restart all child processes if server process crashed
		intensity => 1, 
        period => 5},
    ChildSpecs = [
        #{id => docking_server,
		   start => {docking_server, start_link, [DockingStationDbRef]},
           restart => permanent,
		   shutdown => 5000,
           modules => [docking_server]},   % needs to clean up, so using default timeout() value
        #{id => station_supervisor,
            start => {station_supervisor, start_link, []},
            restart => permanent,
            shutdown => infinity,   % shutdown must be infinity for supervisor
            worker => supervisor,
            modules => [station_supervisor]},
        #{id => docking_listener,   % start listener only after station supervisor has started
            start => {docking_listener, start_link, []},
            restart => permanent,
            worker => worker,
            modules => [docking_listener]}
        ],
    {ok, {SupFlags, ChildSpecs}}.

start_child(Total, Occupied, Name) ->
    station_supervisor:start_child(Total, Occupied, Name).


% Tests
ev_sup_test_() -> 
    {spawn,
        {setup,
            fun() -> 
                {ok, _} = ev_supervisor:start_link()
            end,
            fun(_) ->
                ok
            end,
            [
                fun() -> 
                    % following the given test
                    {ok, _} = ev_supervisor:start_child(3, 1, kellogg),
                    {ok, _} = ev_supervisor:start_child(3, 2, station),
                    ?assertEqual(docking:release_moped(station), ok),
                    ?assertEqual(docking:release_moped(station), ok),
                    exit(whereis(station), kill),
                    timer:sleep(100),
                    % station's state is restored after restart, no more mopeds dcoked currently
                    ?assertEqual(docking:release_moped(station), {error, empty})
                end,
                ?_assertEqual(docking:find_moped(station), [
                    {kellogg, [{total, 3}, {occupied, 1}, {free, 2}]}
                ]),
                ?_assertEqual(docking:find_moped(kellogg), []),
                ?_assertEqual(docking:find_docking_point(kellogg), [
                    {station, [{total, 3}, {occupied, 0}, {free, 3}]}
                ]),
                fun() ->
                    ?assertEqual(docking:secure_moped(kellogg), ok),
                    ?assertEqual(docking:secure_moped(kellogg), ok),
                    ?assertEqual(docking:find_docking_point(station), [])
                end,
                fun() ->
                    {ok, _} = ev_supervisor:start_child(3, 2, keble),
                    ?assertEqual(length(docking:find_moped(station)), 2)
                end,
                fun() ->
                    exit(whereis(docking_server), kill),
                    timer:sleep(100),
                    {ok, _} = ev_supervisor:start_child(3, 1, kellogg),
                    ?assertEqual(docking:release_moped(kellogg), ok)
                end,
                ?_assertEqual(docking:find_moped(kellogg), [
                    {keble, [{total, 3}, {occupied, 2}, {free, 1}]}
                ])
            ]
        }
    }.

