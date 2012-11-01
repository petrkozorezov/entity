-module(entity).

-include_lib("entity/include/entity.hrl").

%% API
-export([
    start/0,
    stop/0,
    add_type/4,
    remove_type/1,
    child_spec/4,
    apply/3,
    apply/4,
    attach/4,
    detach/3,
    get_obj/2
]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(?MODULE).

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?MODULE).

-spec add_type(type_name(), start_entity_fun(), timeout(), timeout()) -> ok | {error, term()}.
add_type(TypeName, StartEntityFun, SupStopTimeout, ObjStopTimeout) ->
    supervisor:start_child(entity_sup, child_spec(TypeName, StartEntityFun, SupStopTimeout, ObjStopTimeout)).

-spec remove_type(type_name()) -> ok | {error, term()}.
remove_type(TypeName) ->
    case supervisor:terminate_child(entity_sup, TypeName) of
        ok ->
            supervisor:delete_child(entity_sup, TypeName);
        {error, Reason} ->
            {error, Reason}
    end.

-spec child_spec(type_name(), start_entity_fun(), timeout(), timeout()) -> supervisor:child_spec().
child_spec(TypeName, StartEntityFun, SupStopTimeout, ObjStopTimeout)
    when is_atom(TypeName) ->
    {TypeName, {entity_type_sup, start_link, [{
        TypeName, StartEntityFun, SupStopTimeout, ObjStopTimeout
    }]}, permanent, SupStopTimeout, supervisor, [entity_type_sup]}.

-spec apply(type_name(), id(), fun((pid()) -> any())) ->
    term() | {error, Reason::term()}.
apply(TypeName, Id, Fun) ->
    apply(TypeName, Id, Fun, undefined).

-spec apply(type_name(), id(), fun((pid()) -> any()), term()) ->
    term() | {error, Reason::term()}.
apply(TypeName, Id, Fun, Args) ->
    {ok, Pid} = entity_manager:get_pid_or_spawn(TypeName, Id),
    do_apply(Pid, Fun, Args, 3).

-spec attach(type_name(), id(), role(), term()) ->
    {ok, pid()} | {error, already_attached | {deny, term()} | term()}.
attach(TypeName, Id, Role, Args)->
    do_attach(TypeName, Id, Role, Args, 10).

-spec detach(type_name(), id(), role()) -> ok | {error, not_attached}.
detach(TypeName, Id, Role) ->
    case entity_manager:get_pid(TypeName, Id) of
        {ok, Pid} ->
            entity_obj:detach(Pid, Role);
        {error, not_registered_uid} ->
            {error, not_attached}
    end.

-spec get_obj(type_name(), id()) -> {ok, pid()} | {error, term()}.
get_obj(TypeName, Id) ->
    entity_manager:get_pid_or_spawn(TypeName, Id).

do_attach(_TypeName, _Id, _Role, _Args, 0) ->
    exit(attach_attemps_limit_hit);
do_attach(TypeName, Id, Role, Args, N) ->
    {ok, Pid} = entity_manager:get_pid_or_spawn(TypeName, Id),
    try entity_obj:attach(Pid, Role, Args) of
        ok ->
            {ok, Pid};
        Error = {error, _} ->
            Error
    catch exit:{noproc, _} ->
        do_attach(TypeName, Id, Role, Args, N - 1)
    end.


do_apply(_, _, _, 0) ->
    exit(apply_attemps_limit_hit);
do_apply(Pid, Fun, Args, N) ->
    try
        Fun(Pid)
    catch
        exit:{noproc, _} ->
            do_apply(Pid, Fun, Args, N - 1)
    end.
