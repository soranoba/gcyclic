

# Module gcyclic_supervisor #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The server supervise the cyclic message passing group.

Copyright (c) 2016 Hinagiku Soranoba All Rights Reserved.

__Behaviours:__ [`supervisor`](supervisor.md).

__This module defines the `gcyclic_supervisor` behaviour.__<br /> Required callback functions: `init/1`.

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-strategy">strategy()</a> ###


<pre><code>
strategy() = one_for_all
</code></pre>




### <a name="type-sup_flags">sup_flags()</a> ###


<pre><code>
sup_flags() = {<a href="#type-strategy">strategy()</a>, Intensity::non_neg_integer(), Period::pos_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Start and link a gcyclic supervisor.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(CallbackModule::module(), Args::term()) -&gt; {ok, pid()} | ignore | {error, StartLinkErr}
</code></pre>

<ul class="definitions"><li><code>StartLinkErr = {already_started, pid()} | {shutdown, term()} | term()</code></li></ul>

Start and link a gcyclic supervisor.

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(SupName, CallbackModule::module(), Args::term()) -&gt; {ok, pid()} | ignore | {error, StartLinkErr}
</code></pre>

<ul class="definitions"><li><code>SupName = {local, Name} | {global, GlobalName} | {via, module(), ViaName}</code></li><li><code>Name = atom()</code></li><li><code>GlobalName = atom()</code></li><li><code>ViaName = term()</code></li><li><code>StartLinkErr = {already_started, pid()} | {shutdown, term()} | term()</code></li></ul>

__See also:__ [start_link/2](#start_link-2).

