-module(entity_obj).
-behaviour(gen_server).

-include_lib("entity/include/entity.hrl").

%% API
-export([
    start_link/3,
    start_link/4,
    call/2,
    cast/2,
    attach/3,
    attach/4,
    detach/2,
    detach/3,
    is_attached/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-export_type([
    id/0,
    role/0,
    attached/0,
    reason/0,
    option/0
]).
-type save_strategy() :: list(after_requests | after_last_detach).
-type option()  ::   {unload, timeout()}
                   | {hibernate, timeout()}
                   | {save_strategy, save_strategy()}
.
-type options() :: list(option()).

%% TODO: вынести в meck тестовый модуль
%%       вынести сейв из terminate в простое сообщение перед выгрузкой

%% Если хочешь сделать сэйв когда-то ещё -- сделай это сам.
%% Сейв, происходит в terminate'е, это надо понимать.
-callback init(id(), Data::term(), Options::term()) ->
    {ok, entity_state()} | {error, term()}.

-callback handle_attach(id(), pid(), role(), term(), attached(), entity_state()) ->
      {ok, entity_state(), attach_context()}
    | {deny, reason(), entity_state()}.

-callback handle_detach(id(), pid(), role(), attached(), entity_state()) ->
    {ok, entity_state()}.

-callback handle_call(id(), term(), pid(), attached(), entity_state()) ->
      {reply, term(), entity_state()}
    | {noreply, entity_state()}
    | {stop, reason(), term(), entity_state()}.

-callback handle_cast(id(), term(), attached(), entity_state()) ->
      {noreply, entity_state()}
    | {stop, reason(), entity_state()}.

-callback handle_info(id(), term(), attached(), entity_state()) ->
      {noreply, entity_state()}
    | {stop, reason(), entity_state()}.

-callback terminate(id(), term(), attached(), entity_state()) ->
    any().

-callback load(id()) ->
    {ok, Data::term()} | {error, Reason::term()}.

-callback prepare_save(id(), entity_state()) ->
    Data::term().

-callback save(id(), Data::term()) ->
    any().


-record(state, {
    id                 :: id(),
    clients      = []  :: list(),
    monitors     = []  :: list(),
    mod                :: module(),
    entity_saved_data  :: entity_state(),
    entity_state       :: entity_state(),
    unload_tref        :: reference(),
    hibernate          :: timeout(),
    unload             :: timeout(),
    save_strategy      :: save_strategy()
}).
-type state()            :: #state{}.

%%
%% API
%%
-spec start_link(module(), id(), term()) ->
    {ok, pid()} | {error, reason()}.
start_link(Mod, Id, Args) ->
    start_link(Mod, Id, Args, []).

-spec start_link(module(), id(), term(), options()) ->
    {ok, pid()} | {error, reason()}.
start_link(Mod, Id, Args, Options) ->
    gen_server:start_link(?MODULE, {Mod, Id, Args, Options}, []).


-spec attach(pid(), role(), term()) -> ok | {error, already_attached | {deny, term()}}.
attach(Pid, Role, Args)->
    attach(Pid, self(), Role, Args).

-spec attach(pid(), pid(), role(), term()) -> ok | {error, already_attached | {deny, term()}}.
attach(EntityPid, ClientPid, Role, Args)->
    gen_server:call(EntityPid, {attach, ClientPid, Role, Args}).

-spec detach(pid(), role()) -> ok | {error, not_attached}.
detach(Pid, Role) ->
    detach(Pid, self(), Role).

-spec detach(pid(), pid(), role()) -> ok | {error, not_attached}.
detach(EntityPid, ClientPid, Role) ->
    gen_server:call(EntityPid, {detach, ClientPid, Role}).

-spec call(pid(), term()) -> term().
call(Pid, Call) ->
    gen_server:call(Pid, {call, Call}).

-spec cast(pid(), term()) -> ok.
cast(Pid, Cast) ->
    gen_server:cast(Pid, {cast, Cast}).

-spec is_attached(pid(), pid(), role()) -> boolean().
is_attached(EntityPid, ClientPid, Role) ->
    gen_server:call(EntityPid, {is_attached, ClientPid, Role}).

%%
%% callbacks
%%
-spec init({module(), id(), term(), options()}) ->
    {ok, state()} | {stop, term()}.
init({Mod, Id, Args, Options}) ->
    process_flag(trap_exit, true),
    Unload       = proplists:get_value(unload,    Options, infinity),
    Hibernate    = proplists:get_value(hibernate, Options, infinity),
    SaveStrategy = proplists:get_value(save_strategy, Options, []  ),
    State = #state{id=Id, mod=Mod, unload=Unload, hibernate=Hibernate, save_strategy=SaveStrategy},
    try Mod:load(Id) of
        {ok, Data} ->
            case Mod:init(Id, Data, Args) of
                {ok, EntityState} ->
                    NewState = State#state{
                        entity_saved_data = Data,
                        entity_state      = EntityState
                    },
                    {ok, try_start_unload(NewState), NewState#state.hibernate};
                {stop, Reason} ->
                    {stop, Reason};
                Other ->
                    {stop, {bad_return_value, Other}}
            end;
        {error, Reason} ->
            {stop, Reason};
        Other ->
            {stop, {bad_return_value, Other}}
    catch T:E ->
        handle_mod_error(State, {T, E}, {init, Args}, erlang:get_stacktrace()),
        {stop, {mod_error, T, E}}
    end.

handle_call({attach, Pid, Role, Args}, _, State) ->
    case is_client(Pid, Role, State) of
        false ->
            {Reply, NewState} = do_attach(Pid, Role, Args, State),
            {reply, Reply, try_start_unload(NewState), NewState#state.hibernate};
        true  ->
            {reply, {error, already_attached},  State}
    end;

handle_call({detach, Pid, Role}, _, State) ->
    case is_client(Pid, Role, State) of
        true   ->
            NewState = do_detach(Pid, Role, State),
            NewState2 = save_if_needed(NewState),
            {reply, ok, try_start_unload(NewState2), NewState#state.hibernate};
        false  -> {reply, {error, not_attached}, try_start_unload(State)}
    end;

handle_call({is_attached, ClientPid, Role}, _, State) ->
    {reply, is_client(ClientPid, Role, State), State};

handle_call({call, Call}, {From, _}, State=#state{mod=Mod}) ->
    try Mod:handle_call(State#state.id, Call, From, State#state.clients, State#state.entity_state) of
        {reply, Reply, NewEntityState} ->
            NewState = State#state{entity_state=NewEntityState},
            {reply, Reply, try_start_unload(try_save(after_requests, NewState)), NewState#state.hibernate};
        {noreply, NewEntityState} ->
            NewState = State#state{entity_state=NewEntityState},
            {noreply, try_start_unload(try_save(after_requests, NewState)), NewState#state.hibernate};
        {stop, Reason, Reply, NewEntityState} ->
            {stop, Reason, Reply, State#state{entity_state=NewEntityState}};
        Other ->
            handle_mod_error(State, {bad_return_value, Other}, {call, Call}, erlang:get_stacktrace()),
            {noreply, try_start_unload(State), State#state.hibernate}
    catch T:E ->
        handle_mod_error(State, {T, E}, {call, Call}, erlang:get_stacktrace()),
        {noreply, State}
    end;

handle_call(Msg, {From, _}, State) ->
    error_logger:error_msg("[~p]: unexpected call received '~p' from '~p'", [State#state.id, Msg, From]),
    {noreply, try_start_unload(State), State#state.hibernate}.


handle_cast({cast, Cast}, State=#state{mod=Mod}) ->
    try Mod:handle_cast(State#state.id, Cast, State#state.clients, State#state.entity_state) of
        {noreply, NewEntityState} ->
            NewState = State#state{entity_state=NewEntityState},
            {noreply, try_start_unload(try_save(after_requests, NewState)), State#state.hibernate};
        {stop, Reason, NewEntityState} ->
            {stop, Reason, State#state{entity_state=NewEntityState}};
        Other ->
            handle_mod_error(State, {bad_return_value, Other}, {cast, Cast}, erlang:get_stacktrace()),
            {noreply, try_start_unload(State), State#state.hibernate}
    catch T:E ->
        handle_mod_error(State, {T, E}, {cast, Cast}, erlang:get_stacktrace()),
        {noreply, try_start_unload(State), State#state.hibernate}
    end;

handle_cast(Msg, State) ->
    error_logger:error_msg("[~p]: unexpected cast received: '~p'", [State#state.id, Msg]),
    {noreply, try_start_unload(State), State#state.hibernate}.


handle_info(Info={'DOWN', Ref, process, From, _}, State) ->
    case lists:keyfind(Ref, 2, State#state.monitors) of
        false ->
            do_handle_info(Info, State);
        _ ->
            NewState =
                lists:foldl(
                    fun ({Pid, Role, _}, AccState) when (Pid==From) ->
                            do_detach(Pid, Role, AccState);
                        ({_, _, _}, AccState) ->
                            AccState
                    end,
                    State,
                    State#state.clients
                ),
            {noreply, try_start_unload(NewState), State#state.hibernate}
    end;

handle_info({timeout, _, unload}, State=#state{}) ->
    {stop, normal, State};
handle_info(timeout, State=#state{}) ->
    {noreply, State, hibernate};
handle_info(Info, State=#state{}) ->
    do_handle_info(Info, State).    

code_change(_OldVsn, State, _Extra) ->
    {ok, try_start_unload(State)}.

terminate(Reason, State=#state{mod=Mod}) ->
    NewState = save(State),
    try
        Mod:terminate(NewState#state.id, Reason, NewState#state.clients, NewState#state.entity_state)
    catch T:E ->
        handle_mod_error(NewState, {T, E}, {terminate, Reason}, erlang:get_stacktrace()),
        ok
    end.

%%
%% Local
%%
do_attach(Pid, Role, Args, State=#state{mod=Mod}) ->
    try Mod:handle_attach(State#state.id, Pid, Role, Args, State#state.clients, State#state.entity_state) of
        {ok, NewEntityState, AttachContext} ->
            NewMonitors =
                case lists:keyfind(Pid, 1, State#state.monitors) of
                    false ->
                        Ref = erlang:monitor(process, Pid),
                        [{Pid, Ref} | State#state.monitors];
                    _ ->
                        State#state.monitors
                end,
            {ok, State#state{
                entity_state=NewEntityState,
                clients=add_attached({Pid, Role, AttachContext}, State#state.clients),
                monitors=NewMonitors
            }};
        {deny, Reason, NewEntityState} ->
            {{error, {deny, Reason}}, State#state{entity_state=NewEntityState}};
        Other ->
            handle_mod_error(State, {bad_return_value, Other}, {attach, Pid, Role}, erlang:get_stacktrace()),
            {{deny, {bad_return_value, Other}}, State}
    catch T:E ->
        handle_mod_error(State, {T, E}, {attach, Pid, Role}, erlang:get_stacktrace()),
        {{deny, {bad_return_value, {T,E}}}, State}
    end.

do_detach(Pid, Role, State=#state{mod=Mod, monitors=Monitors}) ->
    NewClients = delete_attached(Pid, Role, State#state.clients),
    ClientRoles = get_roles(Pid, State),
    NewMonitors =
        case lists:keyfind(Pid, 1, Monitors) of
            {Pid, Ref} when ClientRoles==[] ->
                erlang:demonitor(Ref),
                lists:delete({Pid, Ref}, Monitors);
            _ ->
                Monitors
        end,
    NewState = State#state{clients=NewClients, monitors=NewMonitors},
    try Mod:handle_detach(NewState#state.id, Pid, Role, NewClients, NewState#state.entity_state) of
        {ok, NewEntityState} ->
            NewState#state{entity_state=NewEntityState};
        Other ->
            handle_mod_error(State, {bad_return_value, Other}, {detach, Pid, Role}, erlang:get_stacktrace()),
            NewState
    catch T:E ->
        handle_mod_error(State, {T, E}, {detach, Pid, Role}, erlang:get_stacktrace()),
        NewState
    end.


do_handle_info(Info, State=#state{mod=Mod}) ->
    try Mod:handle_info(State#state.id, Info, State#state.clients, State#state.entity_state) of
        {noreply, NewEntityState} ->
            NewState = State#state{entity_state=NewEntityState},
            {noreply, try_start_unload(NewState), NewState#state.hibernate};
        {stop, Reason, NewEntityState} ->
            {stop, Reason, State#state{entity_state=NewEntityState}};
        Other ->
            handle_mod_error(State, {bad_return_value, Other}, {info, Info}, erlang:get_stacktrace()),
            {noreply, try_start_unload(State), State#state.hibernate}
    catch T:E ->
        handle_mod_error(State, {T, E}, {info, Info}, erlang:get_stacktrace()),
        {noreply, try_start_unload(State), State#state.hibernate}
    end.

is_client(Pid, Role, State) ->
    find_attached(Pid, Role, State#state.clients) /= false.

get_roles(ClientPid, State) ->
    {_, Roles, _} = lists:unzip3(lists:filter(fun({Pid, _, _}) -> Pid == ClientPid end, State#state.clients)),
    Roles.

save(State=#state{mod=Mod}) ->
    try
        Data = Mod:prepare_save(State#state.id, State#state.entity_state),
        case (Data == State#state.entity_saved_data) of
            true ->
                State;
            false ->
                Mod:save(State#state.id, Data),
                State#state{entity_saved_data=Data}
        end
    catch T:E ->
        handle_mod_error(State, {T, E}, save, erlang:get_stacktrace()),
        State
    end.

save_if_needed(State=#state{clients=[]}) ->
    try_save(after_last_detach, State);
save_if_needed(State=#state{}) ->
    State.

try_save(Event, State=#state{save_strategy=SaveStrategy}) ->
    case lists:member(Event, SaveStrategy) of
        true -> save(State);
        false -> State
    end.

try_start_unload(State=#state{unload=infinity}) ->
    State;
try_start_unload(State) ->
    %% отменить таймер, если был
    case State#state.unload_tref of
        undefined ->
            ok;
        TRef ->
            erlang:cancel_timer(TRef)
    end,
    %% проверить можно ли выгружаться
    %% поставить новый (или не ставить)
    case State#state.clients of
        [] ->
            State#state{unload_tref=erlang:start_timer(State#state.unload, self(), unload)};
        _ ->
            State#state{unload_tref=undefined}
    end.

handle_mod_error(State, Reason, When, Stacktrace) ->
    error_logger:error_msg(
        "[~p]: handle error ~p~n"
        "** When: ~p~n"
        "** State: ~p~n"
        "** Stacktrace: ~p~n",
        [State#state.id, Reason, When, State#state.entity_state, Stacktrace]
    ).

find_attached(_, _, []) ->
    false;
find_attached(Pid0, Role0, [V={Pid1, Role1, _}|_])
    when (Pid0 == Pid1) and (Role0 == Role1) ->
    V;
find_attached(Pid, Role, [_|T]) ->
    find_attached(Pid, Role, T).

add_attached(Attached, AttachedList) ->
    [Attached|AttachedList].

delete_attached(Pid, Role, AttachedList) ->
    case find_attached(Pid, Role, AttachedList) of
        false ->
            AttachedList;
        Attached ->
            lists:delete(Attached, AttachedList)
    end.
