

# Module gcyclic_server #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Cyclic message passing gen_server.

Copyright (c) 2016 Hinagiku Soranoba All Rights Reserved.

__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `gcyclic_server` behaviour.__<br /> Required callback functions: `init/1`, `sync/2`, `handle_call/3`, `handle_cast/2`, `handle_info/2`, `terminate/2`, `code_change/3`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>Start and link a gcyclic gen_server.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(CallbackModule::module(), Args::term(), Options::[term()]) -&gt; {ok, pid()} | ignore | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = {already_started, pid()} | term()</code></li></ul>

Start and link a gcyclic gen_server.

Options is the options of `gen_server:start_link/3`.

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(ServerName, CallbackModule::module(), Args::term(), Options::[term()]) -&gt; {ok, pid()} | ignore | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>ServerName = {local, Name} | {global, GlobalName} | {via, module(), ViaName}</code></li><li><code>Name = atom()</code></li><li><code>GlobalName = atom()</code></li><li><code>ViaName = term()</code></li><li><code>Reason = {already_started, pid()} | term()</code></li></ul>

__See also:__ [start_link/3](#start_link-3).

