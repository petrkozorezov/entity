-module(entity_sup).
-behaviour(supervisor).

-include_lib("entity/include/entity.hrl").

-export([start_link/1]).
-export([init/1]).


-spec start_link([]) -> ok | {error, term()}.
start_link([]) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_one, 10, 100},[]}}.
