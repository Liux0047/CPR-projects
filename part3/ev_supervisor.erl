-module(ev_supervisor).
-behaviour(supervisor).
-include_lib("eunit/include/eunit.hrl").

-export([init/1, start_child/3, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    SupFlags = #{strategy => rest_for_one,   % restart all child processes if server process crashed
		intensity => 1, 
        period => 5},
    ChildSpecs = [
        #{id => docking_server,
		   start => {docking_server, start_link, []},
           restart => permanent,
		   shutdown => 5000},   % needs to clean up, so using default timeout() value
        #{id => station_supervisor,
            start => {station_supervisor, start_link, []},
            restart => permanent,
            shutdown => infinity,   % shutdown must be infinity for supervisor
            worker => supervisor}   
        ],
    {ok, {SupFlags, ChildSpecs}}.

start_child(Total, Occupied, Name) ->
    station_supervisor:start_child(Total, Occupied, Name).


% Tests
given_test_() -> 
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
                    {ok, _} = ev_supervisor:start_child(3, 1, kellogg),
                    {ok, _} = ev_supervisor:start_child(3, 2, station),
                    ?assertEqual(docking:release_moped(station), ok),
                    ?assertEqual(docking:release_moped(station), ok),
                    exit(whereis(station), kill),
                    timer:sleep(100),
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
                ?_assertEqual(docking:find_moped(kellogg), [])
            ]
        }
    }.

