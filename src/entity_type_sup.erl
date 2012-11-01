-module(entity_type_sup).
-behaviour(supervisor).

-include_lib("entity/include/entity.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD_WORKER(Mod, Args),     {Mod, {Mod, start_link, Args}, permanent, 5000, worker,     [Mod]}).
-define(CHILD_SUPERVISOR(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, 5000, supervisor, [Mod]}).

-type options() :: {atom(), term()}.

%%
%% API functions
%%
-spec start_link(options()) -> ok | {error, term()}.
start_link(Options) ->
    supervisor:start_link(?MODULE, Options).

%%
%% supervisor callbacks
%%
-spec init(options()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()}, [supervisor:child_spec()]}}.
init({EntityTypeName, EntityStartFun}) ->
    WorkerSupName = gen_sup_name(EntityTypeName),
    {ok, {{one_for_all, 10, 100},[
        ?CHILD_SUPERVISOR(entity_worker_sup, [{EntityTypeName, WorkerSupName, EntityStartFun}]),
        ?CHILD_WORKER(entity_manager, [EntityTypeName, WorkerSupName])
    ]}}.

gen_sup_name(EntityTypeName) ->
    list_to_atom(atom_to_list(EntityTypeName) ++ "_sup").
