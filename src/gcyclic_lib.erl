%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Utility library
%% @private

-module(gcyclic_lib).

-include("gcyclic_internal.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         strategy/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Return the {@link gcyclic_supervisor:strategy/0} of the `gcyclic_supervisor'.
-spec strategy(pid()) -> gcyclic_supervisor:strategy() | undefined.
strategy(Pid) when node() =:= node(Pid) ->
    case process_info(Pid, dictionary) of
        undefined  -> undefined;
        {_, Dicts} -> proplists:get_value(?Strategy, Dicts)
    end;
strategy(Pid) ->
    case rpc:call(node(Pid), erlang, process_info, [Pid, dictionary]) of
        {badrpc, Reason} -> exit(Reason, [Pid]);
        undefined        -> undefined;
        {_, Dicts}       -> proplists:get_value(?Strategy, Dicts)
    end.
