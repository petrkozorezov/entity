-ifndef(_entity_included).
-define(_entity_included, yeah).

-type id()           :: term().
-type type_name()    :: atom().
-type role()         :: atom().
-type attached()     :: list({pid(), role()}).
-type entity_state() :: term().
-type reason()       :: term().

-type start_entity_fun() :: {module(), atom(), list()}. %%fun((uid()) -> {ok, pid()} | {error, term()}) 

-endif.
