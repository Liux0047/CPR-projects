-module(station_supervisor).
-behaviour(supervisor).

-export([
    start_link/0, init/1, start_child/3
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => simple_one_for_one,    % all children are dynamically created
		intensity => 1, 
        period => 5},
    ChildSpecs = #{id => docking,
		   start => {docking, start_link, []},
           restart => transient,
		   shutdown => brutal_kill,
           modules => [docking]},
    {ok, {SupFlags, [ChildSpecs]}}.

start_child(Total, Occupied, Name) ->
    supervisor:start_child(?MODULE, [Total, Occupied, Name]).