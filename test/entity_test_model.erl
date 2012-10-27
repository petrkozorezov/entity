%% For test purposes only !
-module(entity_test_model).
-behaviour(entity).

%% TYPES
-record(state, {
    name :: term(),
    data :: term()
}).

% -type state() :: #state{}.
%% API
-export([start_link/1]).

%% Entity callbacks
-export([
    init/3,
    handle_attach/6,
    handle_detach/5,
    handle_call/5,
    handle_cast/4,
    handle_info/4,
    terminate/4,
    load/1,
    prepare_save/2,
    save/2
]).

%%
%% API
%%
-spec start_link(entity:id()) ->
    {ok, pid()} | {error, term()}.
start_link(Id) ->
    entity:start_link(?MODULE, Id, test_start_arg, [{unload, 100}, {hibernate, 50}]).

%%
%% entity callbacks
%%
init(Id, Data, test_start_arg) ->
    {ok, #state{name=Id, data=Data}}.

handle_attach(_Id, _Pid, wrong_role, test_attach_arg, _Clients, State) ->
    {deny, role_is_invalid, State};
handle_attach(_Id, _Pid, _, test_attach_arg, _Clients, State) ->
    {ok, State}.

handle_detach(_Id, _Pid, _, _Clients, State) ->
    {ok, State}.


handle_call(_Id, hello_call, _From, _Clients, State) ->
    {reply, ok, State}.

handle_cast(_Id, hello_cast, _Clients, State) ->
    {noreply, State}.

handle_info(_Id, _Msg, _Clients, State) ->
    {noreply, State}.


terminate(_Id, _Reason, _Clients, _State) ->
    ok.

load(_Id) ->
    {ok, data}.

prepare_save(_Id, #state{data=Data}) ->
    Data.

save(_Id, data) ->
    ok.
