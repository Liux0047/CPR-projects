%% @author Xiao Liu <liux0047@gmail.com>
%% @doc This supervisor is responsible for starting and restarting docking stations

-module(station_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1, start_child/3]).

%% @doc Starts the supervisor and registers the process name
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Init callback of supervisor. 
%% Uses simple_one_for_one restart strategy since all children are dynamic
-spec init(_Args::term()) -> {ok, term(), term()}.
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

%% @doc Starts a docking station, using supervisor:start_child/2
-spec start_child(Total::number(), Occupied::number(), Name::atom()) -> {ok, pid()}.
start_child(Total, Occupied, Name) ->
    supervisor:start_child(?MODULE, [Total, Occupied, Name]).