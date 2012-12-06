%% Copyright
-module(entity_tests).
-include_lib("eunit/include/eunit.hrl").

-define(ENTITY_TYPE, test_type).
-define(WRONGROLE, wrong_role).
-define(ALLROLES, [correct_role, ?WRONGROLE]).
-define(UIDS, [12, -15, 1000000000000000000000, 0, <<"qwerty">>, "asasas", palmface]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_stop_test() ->
    ok = start(),
    ok = stop(undefined).


clients_test_() ->
    {"Тестируем работоспособность клиентов",
        {setup,
            fun start/0,
            fun stop/1,
            fun(_) ->
                {inorder,
                    [test_attach_detach()]
                }
            end}
    }.

clients_unload_test_() ->
    {"Тестируем работоспособность выгрузки клиентов",
        {setup,
            fun start/0,
            fun stop/1,
            fun(_) ->
                {inorder,
                    [test_unload()]
                }
            end}
    }.

%%
%% SETUP FUNCTIONS
%%
start() ->
    ok = entity:start(),
    {ok, _} = entity:add_type(?ENTITY_TYPE, {entity_test_model, start_link, []}, 5000, 500),
    ok.

stop(_) ->
    ok = entity:remove_type(?ENTITY_TYPE),
    ok = entity:stop().

%%
%% ACTUAL TESTS
%%

test_attach_detach() ->
    [?_assert(attach_detach())].

test_unload() ->
    [?_assert(do_test_unload()), ?_assert(do_test_unload())].

%%
%% LOCALS
%%
attach_detach() ->
    UidRole = [{Uid, Role} || Role <- ?ALLROLES, Uid <- ?UIDS],
    %% Attach
    lists:foreach(
        fun({Uid, Role}) ->
            case entity:attach(?ENTITY_TYPE, Uid, Role, test_attach_arg) of
                {ok, Pid} when is_pid(Pid) and (Role/=?WRONGROLE) ->
                    ?assert(entity_obj:is_attached(Pid, self(), Role)),
                    ?assertError({badmatch,{error, already_attached}},
                        {ok, Pid} = entity:attach(?ENTITY_TYPE, Uid, Role, test_attach_arg)),
                    ?assertMatch(ok, entity_obj:cast(Pid, hello_cast)),
                    ?assertMatch(ok, entity_obj:call(Pid, hello_call)),

                    ?assertMatch(ok, entity:apply(?ENTITY_TYPE, Uid,
                        fun(Pid1) ->
                            ok=entity_obj:call(Pid1, hello_call)
                        end, test_attach_arg)),

                    ?assertMatch(ok, entity_obj:detach(Pid, Role)),
                    ?assertMatch({error, not_attached}, entity_obj:detach(Pid, Role));
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
    {error, not_registered_uid} = entity_manager:get_pid(?ENTITY_TYPE, Uid),
    
    {ok, Pid} = entity:attach(?ENTITY_TYPE, Uid, correct_role, test_attach_arg),

    % ещё не захибернейтился, но жив
    timer:sleep(20),
    true  = erlang:is_process_alive(Pid),
    false = is_hibernated(Pid),

    % захибернейтился и не выгрузился, т.к. есть аттачи
    timer:sleep(100),
    true  = erlang:is_process_alive(Pid),
    true  = is_hibernated(Pid),
    ok    = entity_obj:call(Pid, hello_call),

    ok    = entity_obj:detach(Pid, correct_role),

    % аттачей нет, выгрузился
    timer:sleep(120),
    false  = erlang:is_process_alive(Pid),
    {error, not_registered_uid} = entity_manager:get_pid(?ENTITY_TYPE, Uid),

    true.

is_hibernated(P) ->
    case process_info(P, [current_function, status]) of
    [{current_function, {erlang, hibernate, _}},
     {status, waiting}] ->
        true;
    _ ->
        false
    end.

% stop_wait(Pid, Reason) ->
%     process_flag(trap_exit, true),
%     exit(Pid, Reason),
%     receive 
%         {'EXIT', Pid, Reason} -> ok
%     after
%         1000 -> exit(manager_stop_failed)
%     end,
%     process_flag(trap_exit, false).
