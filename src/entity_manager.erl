-module(entity_manager).
-behaviour(gen_server).

-include_lib("entity/include/entity.hrl").

%% API
-export([
    start_link/2,
    get_pid/2,
    get_pid_or_spawn/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    uid_pid    :: ets:tid(),
    pid_uid    :: ets:tid(),
    entity_sup :: pid()
}).

-type state()     :: #state{}.
-type name()      :: atom() | pid().

-define(SERVER, ?MODULE).


%%
%% API
%%
-spec start_link(atom(), start_entity_fun()) -> {ok, pid()} | {error, term()}.
start_link(Name, SupName) ->
    gen_server:start_link({local, Name}, ?MODULE, SupName, []).

-spec get_pid_or_spawn(name(), id()) -> {ok, pid()}.
get_pid_or_spawn(ManagerName, Uid) ->
    gen_server:call(ManagerName, {get_pid_or_spawn, Uid}).

-spec get_pid(name(), id()) -> {ok, pid()} | {error, not_registered_uid}.
get_pid(ManagerName, Uid) ->
    gen_server:call(ManagerName, {get_pid, Uid}).


%%
%% gen_server callbacks
%%
-spec init(atom()) -> {ok, state()}.
init(SupName) ->
    {ok, #state{
        uid_pid    = ets:new(uids, [set, private]),
        pid_uid    = ets:new(pids, [set, private]),
        entity_sup = SupName
    }}.


-spec handle_call({get_pid_or_spawn, id()}, {pid(), term()}, state()) -> {reply, term(), state()};
                 ({get_pid, id()}, {pid(), term()}, state()) -> {reply, term(), state()};
                 ('none()', {pid(), term()}, state()) -> {noreply, {error, term()}}.
handle_call({get_pid_or_spawn, Uid}, _From, State=#state{uid_pid=U2P, pid_uid=P2U}) ->
    Pid = 
        case ets:lookup(U2P, Uid) of
            [{Uid, Pid1}] -> Pid1;
            [] ->
                {ok, Pid2} = entity_worker_sup:start_child(State#state.entity_sup, Uid),
                ets:insert(U2P, {Uid, Pid2}),
                ets:insert(P2U, {Pid2, Uid}),
                erlang:monitor(process, Pid2),
                Pid2
        end,
    {reply, {ok, Pid}, State};

handle_call({get_pid, Uid}, {_From, _}, State) ->
    U2P   = State#state.uid_pid,
    Reply =
        case ets:lookup(U2P, Uid) of
            [{Uid, Pid}] -> {ok, Pid};
            []           -> {error, not_registered_uid}
        end,
    {reply, Reply, State};

handle_call(Call, From, State) ->
    error_logger:error_msg("unexpected call received: ~p from ~p", [Call, From]),
    {noreply, State}.


-spec handle_cast(term(), state()) ->
    {noreply, state()}.
handle_cast(Cast, State=#state{}) ->
    error_logger:error_msg("unexpected cast received: ~p", [Cast]),
    {noreply, State}.


-spec handle_info({'DOWN', reference(), process, pid(), term()}, state()) -> {noreply, state()};
                 ('none()', state()) -> {noreply, state()}.
handle_info(Info={'DOWN', _Ref, process, Pid, _}, State=#state{uid_pid=U2P, pid_uid=P2U}) ->
    case ets:lookup(P2U, Pid) of
        [{Pid, Uid}] ->
            ets:delete(U2P, Uid),
            ets:delete(P2U, Pid);
        [] ->
            error_logger:error_msg("unexpected info ~p", [Info]),
            ok
    end,
    {noreply, State};

handle_info(Info, State=#state{}) ->
    error_logger:error_msg("unexpected info ~p", [Info]),
    {noreply, State}.


-spec code_change(term() | {down, term()}, state(), term()) ->
    {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec terminate(term() | term(), term()) -> ok.
terminate(_Reason, #state{}) ->
    ok.
