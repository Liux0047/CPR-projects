%% @author Xiao Liu <liux0047@gmail.com>
%% @doc This supervisor is the entry point to the application 
%% It is supervising docking_server, docking_listener and station_supervisor

-module(ev_supervisor).
-behaviour(supervisor).
-include_lib("eunit/include/eunit.hrl").

-export([init/1, start_child/3, start_link/0]).

%% @doc Start the supervisor and registers the process name
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Init callback of supervisor OTP behaviour. 
%% Creates an ETS table and pass the table reference to docking_server.
%% Starts docking_server, station_supervisor, station_recoverer, docking_listener sequentially.
-spec init(_Args::term()) -> {ok, term(), term()}.
init(_Args) ->
    % creating ETS in supervisor to avoid losing state when server process crashes
    % not using named_table to avoid access from other processes
    DockingStationDbRef = docking_server:empty(),
    SupFlags = #{strategy => rest_for_one,   % restart all child processes if server process crashed
		intensity => 1, 
        period => 5},
    ChildSpecs = [
        % first start the server to record states
        #{id => docking_server,
		   start => {docking_server, start_link, [DockingStationDbRef]},
           restart => permanent,
		   shutdown => 5000,    % using default timeout() value to allow work be done in terminate 
           modules => [docking_server]},   
        % then stations can be started
        #{id => station_supervisor,
            start => {station_supervisor, start_link, []},
            restart => permanent,
            shutdown => infinity,   % shutdown must be infinity for supervisor
            worker => supervisor,
            modules => [station_supervisor]},
        #{id => station_recoverer,
            start => {station_recoverer, start_link, []},
            restart => transient,
            worker => worker,
            modules => [station_recoverer, docking_server]},
        % start listener only after station supervisor has started
        #{id => docking_listener,   
            start => {docking_listener, start_link, []},
            restart => permanent,
            worker => worker,
            modules => [docking_listener]}
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% @doc Start a station within this supervision tree. 
%% If the stations has been previously started, use its state in ETS table
-spec start_child(Total::number(), Occupied::number(), Name::atom()) -> {ok, pid()}.
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
                % kellogg should have mopeds
                ?_assertEqual(docking:find_moped(station), [
                    {kellogg, [{total, 3}, {occupied, 1}, {free, 2}]}
                ]),
                % no more mopeds in station
                ?_assertEqual(docking:find_moped(kellogg), []),
                % station has 3 docking points at the moment
                ?_assertEqual(docking:find_docking_point(kellogg), [
                    {station, [{total, 3}, {occupied, 0}, {free, 3}]}
                ]),
                % no more docking point in kellogg after securing two
                fun() ->
                    ?assertEqual(docking:secure_moped(kellogg), ok),
                    ?assertEqual(docking:secure_moped(kellogg), ok),
                    ?assertEqual(docking:find_docking_point(station), [])
                end,
                % start another station, find_moped should now find 2 stations with mopeds
                fun() ->
                    {ok, _} = ev_supervisor:start_child(3, 2, keble),
                    ?assertEqual(length(docking:find_moped(station)), 2)
                end,
                % crash the docking_server, verfiy that the state can be restored
                fun() ->
                    exit(whereis(docking_server), kill),
                    timer:sleep(100),
                    ?assertEqual(docking:get_info(kellogg), 
                        {kellogg, [{total, 3}, {occupied, 3}, {free, 0}]})
                end,
                ?_assertEqual(docking:find_moped(kellogg), [
                    {keble, [{total, 3}, {occupied, 2}, {free, 1}]}
                ])
            ]
        }
    }.

