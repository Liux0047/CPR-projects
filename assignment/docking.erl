-module(docking).
-behaviour(gen_statem).

-export([start_link/3]).
-export([
    init/1, callback_mode/0, terminate/3,
    release_moped/1,
    empty/3, idle/3
]).

start_link(Total, Occupied, Name) ->
    gen_statem:start_link({local,Name}, ?MODULE, {Total, Occupied}, []).

init({Total, Occupied}) ->
    {ok, empty, {Total, Occupied}}. 

empty({call, {From, _}}, release_moped, Data) ->
    io:format("empty state: keep_state_and_data from ~p", [From]),
    From ! {error, empty},
    keep_state_and_data;
empty({call, {From, _}}, secure_moped, Data) ->
    {next_state, idle, Data}.

idle({call, _}, release_moped, Data) ->
    ok.

full() ->
    ok.

release_moped(Name) ->
    gen_statem:call(Name, release_moped),
    receive
        {error, empty} ->
            io:format("received {error, empty}"),
            {error, empty};
        _ ->
            ok
    end.

secure_moped(Name) ->
    gen_statem:call(Name, secure_moped),
    ok.

callback_mode() ->
    state_functions.


terminate(_Reason, State, _Data) ->
    ok.
