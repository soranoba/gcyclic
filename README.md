gcyclic
=======
[![Build Status](https://travis-ci.org/soranoba/erlup.svg?branch=master)](https://travis-ci.org/soranoba/gcyclic)

Generate cyclic message passing server.

## Overview

We sometimes want to create process groups that  do the message passing to each other. (`A <=> B <=> C <=> A`)

There is also a way to use the process name, but this will provide the other way.

## Usage

```erlang
%% rebar.config (rebar3)
{deps, [
        {gcyclic, ".*", {git, "git://github.com/soranoba/gcyclic.git", {branch, "master"}}}
       ]}.
```

```erlang
%% supervisor
-module(sample_supervisor).
-behaviour(gcyclic_supervisor).

start_link() ->
    gcyclic_supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 5, 10},
          [
           {Mod1, {Mod1, start_link, []}, permanent, 5000, worker, [Mod1]},
           {Mod2, {Mod2, start_link, []}, permanent, 5000, worker, [Mod2]},
           {Mod3, {Mod3, start_link, []}, permanent, 5000, worker, [Mod3]},
          ]}}.
```

- [gcyclic_server sample](ct/sample_gcyclic_server.erl)

## Attention

#### `strategy` only support the `one_for_all`.

Because, It is difficult to generalize when only part of processes restarts.

Also, change of strategy by the appup can not support.

#### `start_child` MUST NOT be used.

When you allow start_child, it need to consider that there exist only the part of processes.
