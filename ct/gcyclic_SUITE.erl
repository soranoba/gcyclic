%% @copyright 2016 Hinagiku Soranoba All Rights Reserved.

-module(gcyclic_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(Sup, sample_gcyclic_supervisor).
-define(Child(Id, Module), {Id, {Module, start_link, []}, permanent, 5000, worker, [Module]}).
-define(ALL_TEST, [start, restart]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'common_test' Callback API
%%----------------------------------------------------------------------------------------------------------------------

all() ->
    [
     {group, sample_gcyclic_server}
    ].

groups() ->
    lists:filtermap(fun({group, Mod}) -> {true, {Mod, [], ?ALL_TEST}};
                       (_)            -> false
                    end, all()).

init_per_group(Group, Config) ->
    ok = meck:new(?Sup, [no_history, non_strict, no_link]),
    ok = meck:expect(?Sup, init, 1,
                     {ok, {{one_for_all, 10, 10},
                           [?Child(1, Group), ?Child(2, Group), ?Child(3, Group)]}}),
    Config.

end_per_group(_, Config) ->
    _ = meck:unload(),
    Config.

init_per_testcase(_, Config) ->
    [{watchdog, test_server:timetrap(?t:seconds(30))} | Config].

end_per_testcase(_Case, Config) ->
    test_server:timetrap_cancel(?config(watchdog, Config)),
    ok.

%%----------------------------------------------------------------------------------------------------------------------
%% Test Cases
%%----------------------------------------------------------------------------------------------------------------------

start(_Config) ->
    {ok, SupPid} = gcyclic_supervisor:start_link(?Sup, []),
    SrvPids = [Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid)],
    ok = lists:foreach(fun check_state/1, SrvPids),
    ensure_exited(SupPid).

restart(_Config) ->
    {ok, SupPid} = gcyclic_supervisor:start_link(?Sup, []),
    ok = lists:foreach(fun(N) ->
                               SrvPids0 = [Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid)],
                               exit(lists:nth(N, SrvPids0), restart),
                               ct:sleep(100),
                               SrvPids1 = [Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid)],
                               ok = lists:foreach(fun check_state/1, SrvPids1)
                       end, [1,2,3]),
    ensure_exited(SupPid).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

-spec ensure_exited(pid()) -> ok.
ensure_exited(Pid) ->
    Old = process_flag(trap_exit, true),
    link(Pid),
    exit(Pid, shutdown),
    receive
        {'EXIT', Pid, Reason} -> ?assertEqual(shutdown, Reason)
    end,
    _ = process_flag(trap_exit, Old),
    ok.

-spec check_state(pid()) -> ok.
check_state(P) ->
    P ! {self(), group},
    receive
        Msg ->
            ?assertMatch([{_, P1}, {_, P2}] when is_pid(P1) and is_pid(P2), Msg),
            ok
    end.
