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
    case ets:insert_new(docking_stations, {StationName, Total, Occupied}) of
        true -> 
            % if there's no entry in ets
            {reply, {Total, Occupied}, []};
        false -> 
            % otherwise use ets's record to recreate the station
            [{_, T, O}] = ets:lookup(docking_stations, StationName),
            {reply, {T, O}, []}
    end.

update_station(Total, Occupied, StationName) ->
    gen_server:cast(?MODULE, {update, {Total, Occupied, StationName}}).

% station state update could be asynchronous
handle_cast({update, {Total, Occupied, StationName}}, _) ->
    ets:insert(docking_stations, {StationName, Total, Occupied}), 
    {noreply, []};
handle_cast(stop, _) ->
    {stop, normal, []}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(_Reason, _Data) ->
    ets:delete(docking_stations),
    ok.

