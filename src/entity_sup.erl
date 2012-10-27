-module(entity_sup).
-behaviour(supervisor).

-include_lib("entity/include/entity.hrl").

%% API
-export([start_link/1, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-type options() :: start_entity_fun().

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link(options()) -> ok | {error, term()}.
start_link(Options) ->
    supervisor:start_link(?MODULE, Options).

-spec start_child(pid(), id()) -> ok | {error, term()}.
start_child(Sup, Uid) ->
    supervisor:start_child(Sup, [Uid]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init(options()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()}, [supervisor:child_spec()]}}.
init(StartFunc={Mod, _, _}) ->
    ChildSpecs = {Mod, StartFunc, temporary, 500, worker, [Mod]},
    {ok, {{simple_one_for_one, 10, 100},[ChildSpecs]}}.
