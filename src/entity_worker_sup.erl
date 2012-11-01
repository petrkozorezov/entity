-module(entity_worker_sup).
-behaviour(supervisor).

-include_lib("entity/include/entity.hrl").

%% API
-export([start_link/1, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-type options() :: {type_name(), atom(), start_entity_fun(), timeout()}.

%%
%% API functions
%%
-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link({_, Name, StartFunc, ObjStopTimeout}) ->
    supervisor:start_link({local, Name}, ?MODULE, {StartFunc, ObjStopTimeout}).

-spec start_child(pid(), id()) -> ok | {error, term()}.
start_child(Sup, Uid) ->
    supervisor:start_child(Sup, [Uid]).

%%
%% supervisor callbacks
%%
-spec init(start_entity_fun()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()}, [supervisor:child_spec()]}}.
init({StartFunc={Mod, _, _}, ObjStopTimeout}) ->
    ChildSpecs = {Mod, StartFunc, temporary, ObjStopTimeout, worker, [Mod]},
    {ok, {{simple_one_for_one, 10, 100},[ChildSpecs]}}.
