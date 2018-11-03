%% @author Xiao Liu <liux0047@gmail.com>
%% @doc This module is a state machine for docking station using gen_statem OTP behaviour.
%% It also provides interface for its client

-module(docking).
-behaviour(gen_statem).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/3, init/1, callback_mode/0, stop/1, terminate/3]).
-export([empty/3, idle/3, full/3]).
-export([
    release_moped/1, secure_moped/1, get_info/1,
    find_moped/1, find_docking_point/1,
    release_moped_remote/2, secure_moped_remote/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The client functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Start a docking station called Name and links to the the caller process using gen_statem:start_link.
%% Also registering this docking station locally with Name.<br/>
%% `Total': Total number of docking stations<br/>
%% `Occupied': Number of docking stations already occupied<br/>
%% `Name': Name of station, also the registered process name<br/>
-spec start_link(Total::number(), Occupied::number(), Name::string()) ->
    {ok, pid()}.
start_link(Total, Occupied, Name) ->
    % use the record in docking_server if such one exists
    {T, O}= docking_server:create_station(Total, Occupied, Name),
    gen_statem:start_link({local,Name}, ?MODULE, {T, O}, []).

%% @doc Init callback of gen_statem; initializing the state of docking station
-spec init({Total::number(), Occupied::number()}) -> {ok, atom(), {number(), number()}}.
init({Total, 0}) ->
    {ok, empty, {Total, 0}};
init({Total, Total}) ->
    {ok, full, {Total, Total}};
init({Total, Occupied}) ->
    {ok, idle, {Total, Occupied}}. 


%% @doc Release a moped from the station. Returns {error, empty} if station is empty
-spec release_moped(Name::atom()) -> ok | {error, empty}.
release_moped(Name) ->
    gen_statem:call(Name, {release_moped, Name}).

%% @doc Secure a moped from the station. Returns {error, full} if station is full
-spec secure_moped(Name::atom()) -> ok | {error, full}.
secure_moped(Name) ->
    gen_statem:call(Name, {secure_moped, Name}).

%% @doc Gets the information about the station
-spec get_info(Name::atom()) -> term().
get_info(Name) ->
    gen_statem:call(Name, {get_info, Name}).

%% @doc Find the list of stations that have mopeds available, excluding the one interrogated
-spec find_moped(Name::atom()) -> list().
find_moped(Name) ->
    lists:map(fun format_response/1, docking_server:find_moped(Name)).

%% @doc Find the list of stations that have docking points available, excluding the one interrogated
-spec find_docking_point(Name::atom()) -> list().
find_docking_point(Name) ->
    lists:map(fun format_response/1, docking_server:find_docking_point(Name)).


release_moped_remote(Name, ServerNode) ->
    call_remote(Name, ServerNode, release_moped).

secure_moped_remote(Name, ServerNode) ->
    call_remote(Name, ServerNode, secure_moped).

call_remote(Name, ServerNode, Action) ->
    {docking_listener, ServerNode} ! {Action, Name, self()},
    receive
        {reply, ok} -> ok;
        {reply, error, Reason} -> {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The Finite State Machine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Empty state of Module:StateName/3 from gen_statem when Occupied = 0
empty({call, From}, {release_moped, _Name}, {_Total, 0}) ->
    gen_statem:reply(From, {error, empty}),
    keep_state_and_data;
empty({call, From}, {secure_moped, Name}, {Total, 0}) ->
    docking_server:update_station(Total, 1, Name),
    gen_statem:reply(From, ok),
    {next_state, idle, {Total, 1}};
empty({call, From}, {get_info, Name}, {Total, 0}) ->
    gen_statem:reply(From, format_response({Name, Total, 0})),
    keep_state_and_data.
    
%% @doc Idle state of Module:StateName/3 from gen_statem when Total > Occupied > 0. 
%% The station will stay in idle state till it reaches empty or full conditino
idle({call, From}, {release_moped, Name}, {Total, Occupied}) ->
    docking_server:update_station(Total, Occupied - 1, Name),
    gen_statem:reply(From, ok),
    case Occupied of
        1 -> {next_state, empty, {Total, 0}};
        _ -> {keep_state, {Total, Occupied -1}}
    end;
idle({call, From}, {secure_moped, Name}, {Total, Occupied}) ->
    docking_server:update_station(Total, Occupied + 1, Name),
    gen_statem:reply(From, ok),
    case Occupied + 1 of
        Total -> {next_state, full, {Total, Total}};
        _ -> {keep_state, {Total, Occupied + 1}}
    end;
idle({call, From}, {get_info, Name}, {Total, Occupied}) ->
    gen_statem:reply(From, format_response({Name, Total, Occupied})),
    keep_state_and_data.

%% @doc Full state of Module:StateName/3 from gen_statem when Total = Occupied
full({call, From}, {secure_moped, _Name}, {_Total, _Occupied}) ->
    gen_statem:reply(From, {error, full}),
    keep_state_and_data;
full({call, From}, {release_moped, Name}, {Total, _Occupied}) ->
    docking_server:update_station(Total, Total -1, Name),
    gen_statem:reply(From, ok),
    {next_state, idle, {Total, Total - 1}};
full({call, From}, {get_info, Name}, {Total, _Occupied}) ->
    gen_statem:reply(From, format_response({Name, Total, Total})),
    keep_state_and_data.


format_response({StationName, Total, Occupied}) ->
    {StationName, [{total, Total}, {occupied, Occupied}, {free, Total - Occupied}]}.

%% @doc Use state_functions for finite state machine
-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

%% @doc Stop the process of docking station called Name
-spec stop(Name::atom()) -> ok.
stop(Name) ->
    gen_statem:stop(Name).

%% @doc Terminate callback for any cleanup
terminate(_Reason, _State, _Data) ->
    ok.

% Tests
given_test() -> 
    [
        {ok, _} = start_link(3,1, kellogg),
        ok = release_moped(kellogg),
        {error, empty} = release_moped(kellogg),
        ok = secure_moped(kellogg),
        ok = secure_moped(kellogg),
        ok = secure_moped(kellogg),
        {error, full} = secure_moped(kellogg),
        {kellogg, [{total, 3}, {occupied, 3}, {free, 0}]} = get_info(kellogg),
        stop(kellogg)
    ].

empty_dock_test() ->
    [
        {ok, _} = start_link(3,0, empty_dock),
        {empty_dock, [{total, 3}, {occupied, 0}, {free, 3}]} = get_info(empty_dock),
        {error, empty} = release_moped(empty_dock),
        ok = secure_moped(empty_dock),
        ok = secure_moped(empty_dock),
        {empty_dock, [{total, 3}, {occupied, 2}, {free, 1}]} = get_info(empty_dock),
        ok = secure_moped(empty_dock),
        {error, full} = secure_moped(empty_dock),
        stop(empty_dock)
    ].

full_dock_test() ->
    [
        docking_server:start_link(),
        {ok, _} = start_link(1,1, full_dock),
        {error, full} = secure_moped(full_dock),
        {full_dock, [{total, 1}, {occupied, 1}, {free, 0}]} = get_info(full_dock),
        stop(full_dock)
    ].
