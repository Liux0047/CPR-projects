-module(docking_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

-export([
    start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2,
    create_station/3, update_station/3, stop/0,
    find_moped/1, find_docking_point/1
]).

start_link(DockingStationDbRef) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, DockingStationDbRef, []).

init(DockingStationDbRef) ->
    process_flag(trap_exit, true), % trapping exit to clean up state
    {ok, DockingStationDbRef}.

create_station(Total, Occupied, StationName) ->
    gen_server:call(?MODULE, {create, {Total, Occupied, StationName}}).

update_station(Total, Occupied, StationName) ->
    gen_server:call(?MODULE, {update, {Total, Occupied, StationName}}).

find_moped(Name) ->
    gen_server:call(?MODULE, {find_moped, Name}).

find_docking_point(Name) ->
    gen_server:call(?MODULE, {find_docking_point, Name}).


% station creation should be synchronous because:
% make sure the updated state is stored before advancing to the next state
% in case of non-existing server or a server crashes before sending reply, calling process will terminate
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
handle_call({find_moped, Name}, _From, DockingStationDbRef) ->
    MS = ets:fun2ms(fun({StationName, Total, Occupied}) when Occupied > 0, StationName /= Name ->
        {StationName, Total, Occupied} end),
    {reply, ets:select(DockingStationDbRef, MS), DockingStationDbRef};
handle_call({find_docking_point, Name}, _From, DockingStationDbRef) ->
    MS = ets:fun2ms(fun({StationName, Total, Occupied}) when Total - Occupied > 0, StationName /= Name ->
        {StationName, Total, Occupied} end),
    {reply, ets:select(DockingStationDbRef, MS), DockingStationDbRef}.


stop() ->
    gen_server:cast(?MODULE, stop).

handle_cast(stop, DockingStationDbRef) ->
    {stop, normal, DockingStationDbRef}.

terminate(_Reason, _DockingStationDbRef) ->
    % ets table is connected with the process that creates them
    % if the supervisor process terminates, the table will be deleted automatically
    ok.

% implementing handle_info/2 could prevent process crash from random messages sent to docking_server
% However this is considered defensive programming, thus not encouraged in concurrent model