-module(station_supervisor).
-behaviour(supervisor).

-export([
    start_link/0, init/1, start_child/3
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    SupFlags = #{strategy => simple_one_for_one,
		intensity => 1, 
        period => 5},
    ChildSpecs = #{id => docking,
		   start => {docking, start_link, []},
		   shutdown => brutal_kill},
    {ok, {SupFlags, [ChildSpecs]}}.

start_child(Total, Occupied, Name) ->
    supervisor:start_child(?MODULE, [Total, Occupied, Name]).