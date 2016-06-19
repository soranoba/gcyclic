%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc The server supervise the cyclic message passing group.
%%

-module(gcyclic_supervisor).
-behaviour(supervisor).

-include("gcyclic_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_link/2,
         start_link/3
        ]).

-export_type([
              strategy/0,
              sup_flags/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Defines & Records & Types
%%----------------------------------------------------------------------------------------------------------------------

-type strategy()  :: one_for_all.
-type sup_flags() :: #{strategy  => strategy(),
                       intensity => non_neg_integer(),
                       period    => pos_integer()}
                   | {strategy(), Intensity :: non_neg_integer(), Period :: pos_integer()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

-callback init(Args :: term()) -> {ok, {sup_flags(), [supervisor:child_spec()]}} | ignore.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Start and link a gcyclic supervisor.
-spec start_link(module(), term()) -> {ok, pid()} | ignore | {error, StartLinkErr} when
      StartLinkErr :: {already_started, pid()} | {shutdown, term()} | term().
start_link(CallbackModule, Args) ->
    supervisor:start_link(?MODULE, {CallbackModule, Args}).

%% @see start_link/2
-spec start_link(SupName, module(), term()) -> {ok, pid()} | ignore | {error, StartLinkErr} when
      SupName      :: {local, Name} | {global, GlobalName} | {via, module(), ViaName},
      Name         :: atom(),
      GlobalName   :: atom(),
      ViaName      :: term(),
      StartLinkErr :: {already_started, pid()} | {shutdown, term()} | term().
start_link(SupName, CallbackModule, Args) ->
    supervisor:start_link(SupName, ?MODULE, {CallbackModule, Args}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @hidden
init({CallbackModule, Args}) ->
    Ret = CallbackModule:init(Args),
    Strategy = case Ret of
                   {ok, {{Strategy0, _, _}, _}}        -> Strategy0;
                   {ok, {#{strategy := Strategy0}, _}} -> Strategy0;
                   {ok, _} -> one_for_one;
                   _       -> undefined
               end,
    _ = put(?Strategy, Strategy),
    case lists:member(Strategy, [undefined, one_for_all]) of
        true  -> Ret;
        false -> error({not_supported_strategy, Strategy}, [{CallbackModule, Args}])
    end.
