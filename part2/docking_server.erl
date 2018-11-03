%% @author Xiao Liu <liux0047@gmail.com>
%% @doc This module records the state of all docking stations using gen_server OPT behaviour
%% It also provides interface for its client

-module(docking_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([create_station/3, update_station/3, get_all_stations/0, stop/0, empty/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The client functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Start the server called docking_server and links to the the caller process using gen_server:start_link.
%% Also registering this docking station locally with docking_server.<br/>
%% A reference of ETS table is required, because the state should be kept even if the process crashes 
-spec start_link(DockingStationDbRef::atom()) -> {ok, pid()}.
start_link(DockingStationDbRef) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DockingStationDbRef, []).

%% @doc Creates the internal data structure that's going to be used for this module.
%% In future if the internal data structure can be changed without affecting client code. 
%% Initially thi should be an empty construct.
-spec empty() -> atom().
empty() ->
    ets:new(docking_stations, [set, public]).

%% @doc Records a station creation; if the station is already created, return its latest state
-spec create_station(Total::number(), Occupied::number(), StationName::atom()) -> 
    {Total::number(), Occupied::number()}.
create_station(Total, Occupied, StationName) ->
    gen_server:call(?MODULE, {create, {Total, Occupied, StationName}}).

%% @doc Records a station update
-spec update_station(Total::number(), Occupied::number(), StationName::atom()) -> ok.
update_station(Total, Occupied, StationName) ->
    gen_server:call(?MODULE, {update, {Total, Occupied, StationName}}).

%% @doc Gets a list of all stations
-spec get_all_stations() -> list().
get_all_stations() ->
    gen_server:call(?MODULE, get_all_stations).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The server callback functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Init callback of gen_server, trapping exit here 
-spec init(DockingStationDbRef::atom()) -> {ok, atom()}.
init(DockingStationDbRef) ->
    process_flag(trap_exit, true), % trapping exit to enable termniate to execute
    {ok, DockingStationDbRef}.

%% @doc Hand synchronous calls to the server.<br/>
%% Station creation/update should be synchronous because to prevent race condition,
%% updated state should be first stored before advancing to the next state. <br/>
handle_call({create, {Total, Occupied, StationName}}, _From, DockingStationDbRef) ->
    case ets:insert_new(DockingStationDbRef, {StationName, Total, Occupied}) of
        true -> 
            % if there's no entry in ets
            {reply, {Total, Occupied}, DockingStationDbRef};
        false -> 
            % otherwise use ets's record to recreate the station
            [{_, T, O}] = ets:lookup(DockingStationDbRef, StationName),
            {reply, {T, O}, DockingStationDbRef}
    end;
handle_call({update, {Total, Occupied, StationName}}, _From, DockingStationDbRef) ->
    ets:insert(DockingStationDbRef, {StationName, Total, Occupied}), 
    {reply, ok, DockingStationDbRef};
handle_call(get_all_stations, _From, DockingStationDbRef) ->
    {reply, traverse_table(DockingStationDbRef, ets:first(DockingStationDbRef)), DockingStationDbRef}.

%% @doc Stop the server.
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% @doc Handle asynchronous call, currently only handling stop message 
handle_cast(stop, DockingStationDbRef) ->
    {stop, normal, DockingStationDbRef}.

%% @doc Terminate the server and do any cleanup needed here
terminate(_Reason, _DockingStationDbRef) ->
    % ets table is connected with the process that creates them
    % if the supervisor process terminates, the table will be deleted automatically
    ok.

% implementing handle_info/2 could prevent process crash from random messages sent to docking_server
% However this is considered defensive programming, thus not encouraged in concurrent model

traverse_table(_DockingStationDbRef, '$end_of_table') ->
    [];
traverse_table(DockingStationDbRef, Current) ->
    [Elem] = ets:lookup(DockingStationDbRef, Current),
    [Elem | traverse_table(DockingStationDbRef, ets:next(DockingStationDbRef, Current))].