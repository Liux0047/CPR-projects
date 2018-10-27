-module(docking).
-behaviour(gen_statem).
-include_lib("eunit/include/eunit.hrl").

-export([
    start_link/3, init/1, callback_mode/0, terminate/3,
    release_moped/1, secure_moped/1, get_info/1,
    empty/3, idle/3, full/3,
    unregister_proc/1
]).

start_link(Total, Occupied, Name) ->
    % use the record in docking_server if such one exists
    {T, O}= docking_server:create_station(Total, Occupied, Name),
    gen_statem:start_link({local,Name}, ?MODULE, {T, O}, []).

init({Total, 0}) ->
    {ok, empty, {Total, 0}};
init({Total, Total}) ->
    {ok, full, {Total, Total}};
init({Total, Occupied}) ->
    {ok, idle, {Total, Occupied}}. 


empty({call, From}, {release_moped, _}, {_, 0}) ->
    gen_statem:reply(From, {error, empty}),
    keep_state_and_data;
empty({call, From}, {secure_moped, Name}, {Total, 0}) ->
    gen_statem:reply(From, ok),
    docking_server:update_station(Total, 1, Name),
    {next_state, idle, {Total, 1}};
empty({call, From}, {get_info, Name}, {Total, 0}) ->
    gen_statem:reply(From, {Name, [
        {total, Total}, {occupied, 0}, {free, Total}
    ]}),
    keep_state_and_data.
    

idle({call, From}, {release_moped, Name}, {Total, Occupied}) ->
    gen_statem:reply(From, ok),
    docking_server:update_station(Total, Occupied - 1, Name),
    case Occupied of
        1 -> {next_state, empty, {Total, 0}};
        _ -> {keep_state, {Total, Occupied -1}}
    end;
idle({call, From}, {secure_moped, Name}, {Total, Occupied}) ->
    gen_statem:reply(From, ok),
    docking_server:update_station(Total, Occupied + 1, Name),
    case Occupied + 1 of
        Total -> {next_state, full, {Total, Total}};
        _ -> {keep_state, {Total, Occupied + 1}}
    end;
idle({call, From}, {get_info, Name}, {Total, Occupied}) ->
    gen_statem:reply(From, {Name, [
        {total, Total}, {occupied, Occupied}, {free, Total - Occupied}
    ]}),
    keep_state_and_data.


full({call, From}, {secure_moped, _}, _) ->
    gen_statem:reply(From, {error, full}),
    keep_state_and_data;
full({call, From}, {release_moped, Name}, {Total, _}) ->
    gen_statem:reply(From, ok),
    docking_server:update_station(Total, Total -1, Name),
    {next_state, idle, {Total, Total - 1}};
full({call, From}, {get_info, Name}, {Total, _}) ->
    gen_statem:reply(From, {Name, [
        {total, Total}, {occupied, Total}, {free, 0}
    ]}),
    keep_state_and_data.


release_moped(Name) ->
    gen_statem:call(Name, {release_moped, Name}).

secure_moped(Name) ->
    gen_statem:call(Name, {secure_moped, Name}).

get_info(Name) ->
    gen_statem:call(Name, {get_info, Name}).


callback_mode() ->
    state_functions.

terminate(_Reason, _State, _Data) ->
    ok.



% Tests
given_test() -> 
    [
        unregister_proc(kellogg),
        unregister_proc(docking_server),
        docking_server:start_link(),
        {ok, _} = start_link(3,1, kellogg),
        ok = release_moped(kellogg),
        {error, empty} = release_moped(kellogg),
        ok = secure_moped(kellogg),
        ok = secure_moped(kellogg),
        ok = secure_moped(kellogg),
        {error, full} = secure_moped(kellogg),
        {kellogg, [{total, 3}, {occupied, 3}, {free, 0}]} = get_info(kellogg),
        docking_server:stop()
    ].

empty_dock_test() ->
    [
        unregister_proc(empty_dock),
        unregister_proc(docking_server),
        docking_server:start_link(),
        {ok, _} = start_link(3,0, empty_dock),
        {empty_dock, [{total, 3}, {occupied, 0}, {free, 3}]} = get_info(empty_dock),
        {error, empty} = release_moped(empty_dock),
        ok = secure_moped(empty_dock),
        ok = secure_moped(empty_dock),
        {empty_dock, [{total, 3}, {occupied, 2}, {free, 1}]} = get_info(empty_dock),
        ok = secure_moped(empty_dock),
        {error, full} = secure_moped(empty_dock),
        docking_server:stop()
    ].

full_dock_test() ->
    [
        unregister_proc(full_dock),
        unregister_proc(docking_server),
        docking_server:start_link(),
        {ok, _} = start_link(1,1, full_dock),
        {error, full} = secure_moped(full_dock),
        {full_dock, [{total, 1}, {occupied, 1}, {free, 0}]} = get_info(full_dock),
        docking_server:stop()
    ].

unregister_proc(Name) ->
    case whereis(Name) of
        undefined -> ok;
        _ -> unregister(Name)
    end.