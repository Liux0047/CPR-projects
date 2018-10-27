-module(docking).
-behaviour(gen_statem).

-export([start_link/3]).
-export([
    init/1, callback_mode/0, terminate/3,
    release_moped/1, secure_moped/1,
    empty/3, idle/3, full/3
]).

start_link(Total, Occupied, Name) ->
    gen_statem:start_link({local,Name}, ?MODULE, {Total, Occupied}, []).

init({Total, 0}) ->
    {ok, empty, {Total, 0}};
init({Total, Occupied}) ->
    {ok, idle, {Total, Occupied}}. 


empty({call, From}, release_moped, {_, 0}) ->
    io:format("empty state | release: keep_state_and_data from ~p~n", [From]),
    gen_statem:reply(From, {error, empty}),
    keep_state_and_data;
empty({call, From}, secure_moped, {Total, 0}) ->
    io:format("empty state | secure: next_state=idle from ~p~n", [From]),
    gen_statem:reply(From, ok),
    {next_state, idle, {Total, 1}}.
    

idle({call, From}, release_moped, {Total, Occupied}) ->
    io:format("idle state | release: Total=~p, Occupied = ~p~n",[Total, Occupied]),
    gen_statem:reply(From, ok),
    case Occupied of
        1 -> {next_state, empty, {Total, 0}};
        _ -> {keep_state, {Total, Occupied -1}}
    end;
idle({call, From}, secure_moped, {Total, Occupied}) ->
    io:format("idle state | secure: Total=~p, Occupied = ~p~n",[Total, Occupied]),
    gen_statem:reply(From, ok),
    case Occupied + 1 of
        Total -> {next_state, full, {Total, Total}};
        _ -> {keep_state, {Total, Occupied + 1}}
    end.


full({call, From}, secure_moped, _) ->
    gen_statem:reply(From, {error, full}),
    keep_state_and_data;
full({call, From}, release_moped, {Total, _}) ->
    gen_statem:reply(From, ok),
    {next_state, idle, {Total, Total - 1}}.


release_moped(Name) ->
    gen_statem:call(Name, release_moped).

secure_moped(Name) ->
    gen_statem:call(Name, secure_moped).


callback_mode() ->
    state_functions.

terminate(_Reason, State, _Data) ->
    ok.
