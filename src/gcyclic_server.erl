%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Cyclic message passing gen_server
%%

-module(gcyclic_server).
-behaviour(gen_server).

-include("gcyclic_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/3,
         start_link/4
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Defines & Records & Types
%%----------------------------------------------------------------------------------------------------------------------

%%----------------------------------------------------------------------------------------------------------------------
%% Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

-callback init(Args) -> {ok, State} | {ok, State, timeout()} | {ok, State, hibernate} | ignore | {error, Reason} when
      Args   :: term(),
      State  :: term(),
      Reason :: term().

-callback sync([{ChildId :: term(), Child :: pid()}], OldState :: term()) -> Ret when
      Ret :: {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_call(Request :: term(), {pid(), Tag :: reference()}, State :: term()) -> Ret when
      Ret      :: {reply, Reply, NewState}
                | {reply, Reply, NewState, timeout()}
                | {reply, Reply, NewState, hibernate}
                | {noreply, NewState}
                | {noreply, NewState, timeout()}
                | {noreply, NewState, hibernate}
                | {stop, Reason, Reply, NewState}
                | {stop, Reason, NewState},
      Reply    :: term(),
      NewState :: term(),
      Reason   :: term().

-callback handle_cast(Request :: term(), State :: term()) -> Ret when
      Ret      :: {noreply, NewState}
                | {noreply, NewState, timeout()}
                | {noreply, NewState, hibernate}
                | {stop, Reason, NewState},
      NewState :: term(),
      Reason   :: term().

-callback handle_info(Info :: timeout | term(), State :: term()) -> Ret when
      Ret      :: {noreply, NewState}
                | {noreply, NewState, timeout()}
                | {noreply, NewState, hibernate}
                | {stop, Reason, NewState},
      NewState :: term(),
      Reason   :: term().

-callback terminate(Reason :: term(), State :: term()) -> term().

-callback code_change(OldVsn, State :: term(), Extra :: term()) -> {ok, NewState} | {error, Reason :: term()} when
      OldVsn   :: Vsn | {down, Vsn},
      Vsn      :: term(),
      NewState :: term().

-ifdef(optional_callback).
-optional_callbacks([format_status/2]).
-callback format_status(Opt, [PDict | State]) -> Status :: term() when
      Opt   :: normal | terminate,
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term().
-endif.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Start and link a gcyclic gen_server.
%%
%% Options is the options of `gen_server:start_link/3'.
%%
-spec start_link(module(), term(), [term()]) -> {ok, pid()} | ignore | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(CallbackModule, Args, Options) ->
    gen_server:start_link(?MODULE, {CallbackModule, self(), Args}, Options).

%% @see start_link/3
-spec start_link(ServerName, module(), term(), [term()]) -> {ok, pid()} | ignore | {error, Reason} when
      ServerName :: {local, Name} | {global, GlobalName} | {via, module(), ViaName},
      Name       :: atom(),
      GlobalName :: atom(),
      ViaName    :: term(),
      Reason     :: {already_started, pid()} | term().
start_link(ServerName, CallbackModule, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, {CallbackModule, self(), Args}, Options).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @hidden
init({CallbackModule, SpawnedBy, Args}) ->
    ok = gen_server:cast(self(), ?Msg({sync, SpawnedBy})),
    _  = put(?Callback, CallbackModule),
    CallbackModule:init(Args).

%% @hidden
handle_call(Msg, From, State) ->
    (get(?Callback)):handle_call(Msg, From, State).

%% @hidden
handle_cast(?Msg({sync, SpawnedBy}), State) ->
    case gcyclic_lib:strategy(SpawnedBy) of
        one_for_all ->
            Groups = lists:filtermap(fun({_, Child, _, _})  when Child =:= self() -> false;
                                        ({Id, Child, _, _}) when is_pid(Child)    -> {true, {Id, Child}}
                                     end, supervisor:which_children(SpawnedBy)),
            case (get(?Callback)):sync(Groups, State) of
                {ok, State1}   -> {noreply, State1};
                {stop, Reason} -> {stop, Reason, State}
            end
    end;
handle_cast(Other, State) ->
    (get(?Callback)):handle_cast(Other, State).

%% @hidden
handle_info(Info, State) ->
    (get(?Callback)):handle_info(Info, State).

%% @hidden
terminate(Reason, State) ->
    (get(?Callback)):terminate(Reason, State).

%% @hidden
code_change(OldVsn, State, Extra) ->
    (get(?Callback)):code_change(OldVsn, State, Extra).

%% @hidden
format_status(Opt, [PDict, State]) ->
    %% NOTE: It adopt the default value, when the function is crashed.
    (get(?Callback)):format_status(Opt, [PDict, State]).
