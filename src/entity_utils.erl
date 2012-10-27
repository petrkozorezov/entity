-module(entity_utils).

-include_lib("entity/include/entity.hrl").

-export([
    apply/3,
    apply/4,
    attach/4,
    detach/3
]).

-spec apply(entity_manager:name(), id(), fun((pid()) -> any())) ->
    term() | {error, Reason::term()}.
apply(ManagerName, Id, Fun) ->
    apply(ManagerName, Id, Fun, undefined).

-spec apply(entity_manager:name(), id(), fun((pid()) -> any()), term()) ->
    term() | {error, Reason::term()}.
apply(ManagerName, Id, Fun, Args) ->
    {ok, Pid} = entity_manager:get_pid_or_spawn(ManagerName, Id),
    do_apply(Pid, Fun, Args, 3).

-spec attach(entity_manager:name(), id(), role(), term()) ->
    {ok, pid()} | {error, already_attached | {deny, term()} | term()}.
attach(ManagerName, Id, Role, Args)->
    do_attach(ManagerName, Id, Role, Args, 10).

-spec detach(entity_manager:name(), id(), role()) -> ok | {error, not_attached}.
detach(ManagerName, Id, Role) ->
    case entity_manager:get_pid(ManagerName, Id) of
        {ok, Pid} ->
            entity:detach(Pid, Role);
        {error, not_registered_uid} ->
            {error, not_attached}
    end.

do_attach(_ManagerName, _Id, _Role, _Args, 0) ->
    exit(attach_attemps_limit_hit);
do_attach(ManagerName, Id, Role, Args, N) ->
    {ok, Pid} = entity_manager:get_pid_or_spawn(ManagerName, Id),
    try entity:attach(Pid, Role, Args) of
        ok ->
            {ok, Pid};
        Error = {error, _} ->
            Error
    catch exit:{noproc, _} ->
        do_attach(ManagerName, Id, Role, Args, N - 1)
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
