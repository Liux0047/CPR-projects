-module(docking_server).
-behaviour(gen_server).

-export([
    start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2,
    create_station/3, update_station/3, stop/0
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ets:new(docking_stations, [set, named_table]),
    {ok, []}.

create_station(Total, Occupied, StationName) ->
    gen_server:call(?MODULE, {create, {Total, Occupied, StationName}}).

% station creation should be synchronous
handle_call({create, {Total, Occupied, StationName}}, _From, _) ->
    Station = ets:lookup(docking_stations, StationName),
    handle_create_station(Station, StationName, Total, Occupied).

update_station(Total, Occupied, StationName) ->
    gen_server:cast(?MODULE, {update, {Total, Occupied, StationName}}).

% station state update could be asynchronous
handle_cast({update, {Total, Occupied, StationName}}, _) ->
    ets:delete(docking_stations, StationName),
    insert_station(StationName, Total, Occupied),
    {noreply, []};
handle_cast(stop, _) ->
    {stop, normal, []}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(_Reason, _Data) ->
    ets:delete(docking_stations),
    ok.

handle_create_station([], StationName, Total, Occupied) ->
    insert_station(StationName, Total, Occupied),
    {reply, ok, []};
handle_create_station([_], _ ,_,  _) ->
    {reply, existed, []}.

insert_station(StationName, Total, Occupied) ->
    ets:insert(docking_stations, {StationName, [
        {total, Total}, {ocuupied, Occupied}, {free, Total - Occupied}
    ]}).

