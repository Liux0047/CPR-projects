-module(ev_supervisor).
-behaviour(supervisor).
-include_lib("eunit/include/eunit.hrl").

-export([init/1, start_child/3, start_link/0]).

start_link() ->
    docking_server:start_link(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    SupFlags = #{strategy => simple_one_for_one,
		intensity => 5, 
        period => 3600},
    ChildSpecs = #{id => docking,
		   start => {docking, start_link, []},
		   shutdown => brutal_kill},
    {ok, {SupFlags, [ChildSpecs]}}.

start_child(Total, Occupied, Name) ->
    supervisor:start_child(?MODULE, [Total, Occupied, Name]).


% Tests
given_test_() -> 
    {spawn,
        {setup,
            fun() -> 
                {ok, _} = ev_supervisor:start_link()
            end,
            fun(_) ->
                docking_server:stop()
            end,
            [
                fun() -> 
                    {ok, _} = ev_supervisor:start_child(3, 1, kellogg),
                    {ok, _} = ev_supervisor:start_child(3, 2, station),
                    ?assertEqual(docking:release_moped(station), ok),
                    ?assertEqual(docking:release_moped(station), ok),
                    exit(whereis(station), kill),
                    timer:sleep(100),
                    ?assertEqual(docking:release_moped(station), {error, empty})
                end,
                ?_assertEqual(docking:find_moped(station), [
                    {kellogg, [{total, 3}, {occupied, 1}, {free, 2}]}
                ])
            ]
        }
    }.

