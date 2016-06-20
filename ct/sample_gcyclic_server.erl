%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(sample_gcyclic_server).
-behaviour(gcyclic_server).

-export([start_link/0, init/1, sync/2, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gcyclic_server:start_link(?MODULE, [], []).

init(_) -> timer:sleep(100), {ok, []}.

sync(X, _) -> {ok, X}.

handle_call(_, _, State) -> {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info({Pid, group}, State) -> Pid ! State, {noreply, State};
handle_info(_, State)            -> {noreply, State}.

terminate(_, _) -> ok.

code_change(_, State, _) -> {ok, State}.
