%% Copyright
-module(entity_tests).
-include_lib("eunit/include/eunit.hrl").

-define(MANAGER, test_manager).
-define(WRONGROLE, wrong_role).
-define(ALLROLES, [correct_role, ?WRONGROLE]).
-define(UIDS, [12, -15, 1000000000000000000000, 0, <<"qwerty">>, "asasas", palmface]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test_() ->
    {"Проверяем запуск и остановку менеджера",
        {setup,
            fun start_mgr/0,
            fun(Pid) ->
                stop_wait(Pid, shutdown)
            end,
            fun(Pid) ->
                {inorder, [is_registered(Pid)]}
            end}
    }.

clients_test_() ->
    {"Тестируем работоспособность клиентов",
        {setup,
            fun start_mgr/0,
            fun(Pid) ->
                stop_wait(Pid, shutdown)
            end,
            fun(_) ->
                {inorder, %% TODO: Add inparallel too
                    [test_attach_detach()]
                }
            end}
    }.

clients_unload_test_() ->
    {"Тестируем работоспособность выгрузки клиентов",
        {setup,
            fun start_mgr/0,
            fun(Pid) ->
                stop_wait(Pid, shutdown)
            end,
            fun(_) ->
                {inorder, %% TODO: Add inparallel too
                    [test_unload()]
                }
            end}
    }.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start_mgr() ->
    {ok, Pid} = entity_manager:start_link(?MANAGER, {entity_test_model, start_link, []}),
    Pid.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%
is_registered(Pid) ->
    [?_assert(is_pid(Pid)),
        ?_assert(erlang:is_process_alive(Pid))].

test_attach_detach() ->
    [?_assert(attach_detach())].

test_unload() ->
    [?_assert(do_test_unload()), ?_assert(do_test_unload())].

%%%%%%%%%%%%
%% LOCALS %%
%%%%%%%%%%%%
attach_detach() ->
    UidRole = [{Uid, Role} || Role <- ?ALLROLES, Uid <- ?UIDS],
    %% Attach
    lists:foreach(
        fun({Uid, Role}) ->
            case entity_utils:attach(?MANAGER, Uid, Role, test_attach_arg) of
                {ok, Pid} when is_pid(Pid) and (Role/=?WRONGROLE) ->
                    ?assert(entity:is_attached(Pid, self(), Role)),
                    ?assertError({badmatch,{error, already_attached}},
                        {ok, Pid} = entity_utils:attach(?MANAGER, Uid, Role, test_attach_arg)),
                    ?assertMatch(ok, entity:cast(Pid, hello_cast)),
                    ?assertMatch(ok, entity:call(Pid, hello_call)),

                    ?assertMatch(ok, entity_utils:apply(?MANAGER, Uid,
                        fun(Pid1) ->
                            ok=entity:call(Pid1, hello_call)
                        end, test_attach_arg)),

                    ?assertMatch(ok, entity:detach(Pid, Role)),
                    ?assertMatch({error, not_attached}, entity:detach(Pid, Role));
                {error, {deny, role_is_invalid}} ->
                    ok
            end
        end,
        UidRole
    ),
    true.

do_test_unload() ->
    % если тайминги уменьшить то тест будет падать из-за рэйсов

    Uid = unload_tester,

    % изначально такого объекта нет
    {error, not_registered_uid} = entity_manager:get_pid(?MANAGER, Uid),
    
    {ok, Pid} = entity_utils:attach(?MANAGER, Uid, correct_role, test_attach_arg),

    % ещё не захибернейтился, но жив
    timer:sleep(20),
    true  = erlang:is_process_alive(Pid),
    false = is_hibernated(Pid),

    % захибернейтился и не выгрузился, т.к. есть аттачи
    timer:sleep(100),
    true  = erlang:is_process_alive(Pid),
    true  = is_hibernated(Pid),
    ok    = entity:call(Pid, hello_call),

    ok    = entity:detach(Pid, correct_role),

    % аттачей нет, выгрузился
    timer:sleep(120),
    false  = erlang:is_process_alive(Pid),
    {error, not_registered_uid} = entity_manager:get_pid(?MANAGER, Uid),

    true.

is_hibernated(P) ->
    case process_info(P, [current_function, status]) of
    [{current_function, {erlang, hibernate, _}},
     {status, waiting}] ->
        true;
    _ ->
        false
    end.

stop_wait(Pid, Reason) ->
    process_flag(trap_exit, true),
    exit(Pid, Reason),
    receive 
        {'EXIT', Pid, Reason} -> ok
    after
        1000 -> exit(manager_stop_failed)
    end,
    process_flag(trap_exit, false).
