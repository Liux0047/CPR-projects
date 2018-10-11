-module(sup).
-export([
    start/1,
    start_child/4,
    stop/1,
    listen/1
]).

start(SupName) ->
    {ok, register(SupName, spawn(sup, listen, [[]]))}.


start_child(SupName, Mod, Func, Args) ->
    SupName ! {create_child, Mod, Func, Args}.

stop(SupName) ->
    exit(whereis(SupName), stop).

listen(State) ->
    process_flag(trap_exit, true),
    receive
        {create_child, Mod, Func, Args} -> 
            NewPid = spawn_link(Mod, Func, Args),
            listen([{NewPid, {Mod, Func, Args}, 5} |State]);
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p has been terminated with reason ~s~n", [Pid, Reason]),
            listen(attempt_restart(Pid, State))
    end.


attempt_restart(_, []) -> [];
attempt_restart(Pid, [{Pid, _ , 0} | T] ) ->
    T;
attempt_restart(Pid, [{Pid, {Mod, Func, Args}, Times} | T] ) ->
    NewPid = spawn_link(Mod, Func, Args),
    [{NewPid, {Mod, Func, Args}, Times - 1} | T];
attempt_restart(Pid, [H | T] ) ->
    [H | attempt_restart(Pid, T)].


