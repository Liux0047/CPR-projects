-module(docking_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

-export([
    start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2,
    create_station/3, update_station/3, stop/0,
    find_moped/1, find_docking_point/1
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    DockingStationsDbRef = ets:new(docking_stations, [set]),
    {ok, DockingStationsDbRef}.

create_station(Total, Occupied, StationName) ->
    gen_server:call(?MODULE, {create, {Total, Occupied, StationName}}).

update_station(Total, Occupied, StationName) ->
    gen_server:cast(?MODULE, {update, {Total, Occupied, StationName}}).

find_moped(Name) ->
    gen_server:call(?MODULE, {find_moped, Name}).

find_docking_point(Name) ->
    gen_server:call(?MODULE, {find_docking_point, Name}).


% station creation should be synchronous
handle_call({create, {Total, Occupied, StationName}}, _From, DockingStationsDbRef) ->
    case ets:insert_new(DockingStationsDbRef, {StationName, Total, Occupied}) of
        true -> 
            % if there's no entry in ets
            {reply, {Total, Occupied}, DockingStationsDbRef};
        false -> 
            % otherwise use ets's record to recreate the station
            [{_, T, O}] = ets:lookup(DockingStationsDbRef, StationName),
            {reply, {T, O}, DockingStationsDbRef}
    end;
handle_call({find_moped, Name}, _From, DockingStationsDbRef) ->
    MS = ets:fun2ms(fun({StationName, Total, Occupied}) when Occupied > 0, StationName /= Name ->
        {StationName, Total, Occupied} end),
    {reply, ets:select(DockingStationsDbRef, MS), DockingStationsDbRef};
handle_call({find_docking_point, Name}, _From, DockingStationsDbRef) ->
    MS = ets:fun2ms(fun({StationName, Total, Occupied}) when Total - Occupied > 0, StationName /= Name ->
        {StationName, Total, Occupied} end),
    {reply, ets:select(DockingStationsDbRef, MS), DockingStationsDbRef}.


% station state update could be asynchronous
handle_cast({update, {Total, Occupied, StationName}}, DockingStationsDbRef) ->
    ets:insert(DockingStationsDbRef, {StationName, Total, Occupied}), 
    {noreply, DockingStationsDbRef};
handle_cast(stop, DockingStationsDbRef) ->
    {stop, normal, DockingStationsDbRef}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(_Reason, DockingStationsDbRef) ->
    ets:delete(DockingStationsDbRef),
    ok.

