%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2004
%%% @end
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------
%%%
%%% @author Vance Shipley <vances@motivity.ca>
%%%
%%% @doc Main API of the LAPD application.
%%%	<p>This module provides the main application programming interface
%%%	for the LAPD application.  The application implements the link
%%% 	access procedures for the D-channel (LAPD) as defined in Q.921.</p>
%%% @end
%%%
%%% @reference <a href="index.html">The LAPD User's Guide</a>
%%%
         
-module(lapd).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').

%% our published API functions
-export([start_link/3, stop/1]).
-export([open/4, close/1]).
-export([bind/3]).


%%----------------------------------------------------------------------
%%  The API functions
%%----------------------------------------------------------------------

%% @spec (Module::atom(), Args::term(), Options::term()) -> Result
%% 	Result = {ok, Sup} | {error, Reason}
%% 	Sup = pid()
%%		Reason = term()
%%
%% @doc Start a new LAPD layer.
%% 	<p>Starts a supervision tree for the new LAPD layer as well as a
%% 	layer management entity process and a multiplexer process.</p>
%%
%% 	<p><tt>Module</tt> is the name of a <a href="lapd_mux_fsm.html">
%% 	<tt>lapd_mux_fsm</tt></a> behaviour callback module which will
%% 	provide the layer 1 adaptation.</p>
%%
%% 	<p><tt>Args</tt> and <tt>Options</tt> have meaning only to the
%% 	callback module but presumably <tt>Args</tt> would identify the
%% 	specific layer 1 service access point for this LAPD layer.</p>
%%
%% 	<p>The callback module will be started with:<br/>
%% 		<tt>gen_fsm:start_link(Module, Args, Options)</tt>
%% 	</p>
%%
%% 	<p>Returns <tt>{ok, Sup}</tt> or <tt>{error, Reason}</tt>.  This
%% 	function is intended to be used as the start function of a
%% 	child specification in an including application's supervision
%% 	tree.</p>
%%
start_link(Module, Args, Options) ->
	case supervisor:start_link(lapd_sup, [Module, Args, Options]) of
		{ok, Sup} ->
			Children = supervisor:which_children(Sup),
			{value, {lme, LME, _, _}} = lists:keysearch(lme, 1, Children),
			{value, {mux, MUX, _, _}} = lists:keysearch(mux, 1, Children),
			gen_server:cast(LME, {mux, MUX}),
			gen_fsm:send_all_state_event(MUX, {lme, LME}),
			{ok, Sup};
		{error, Reason} ->
			{error, Reason}
	end.


%% @spec (Sup::pid()) -> true
%%
%% @doc Stop a running LAPD layer.
%% 	<p>Terminates the top level supervisor for the layer which in turn
%% 	stops all the running processes in the layer.</p>
%%
%% 	<p><tt>Sup</tt> is the pid of the top level supervisor returned
%% 	in a previous call to 
%% 	<a href="#start_link-3"><tt>lapd:start_link/3</tt></a>.</p>
%%
stop(Sup) ->
	exit(Sup, shutdown).


%% @spec (Sup::pid(), SAPI::integer(), TEI::integer(), Options::option_list()) -> {LME, CME, DLE}
%% 	LME = pid()
%% 	CME = pid()
%% 	DLE = pid()
%%
%% @type option_list() = [term()].
%%
%% @doc Open a LAPD service access point.
%% 	<p>Creates a new data link entity (DLE) process.</p>
%%
%% 	<p><tt>Sup</tt> is the pid() of the top level supervisor for a
%% 	layer returned from a previous call to 
%% 	to <a href="#start_link-3"><tt>lapd:start_link/3</tt></a></p>.  
%%
%% 	<p><tt>SAPI</tt> specifies the service access point identifier.
%% 	Valid values are in the range 0..63.</p>
%%
%% 	<p>Some standardized SAPI values are:</p>
%% 	<dl>
%% 		<dt><tt>0</tt></dt> <dd>Call control procedures</dd>
%% 		<dt><tt>16</tt></dt> <dd>Packet communication conforming to
%% 				X.25 level 3 procedures</dd>
%% 		<dt><tt>63</tt></dt> <dd>Layer 2 management procedures</dd>
%% 	</dl>
%%
%% 	<p><tt>TEI</tt> specifies the terminal endpoint identifier for
%% 	non-automatic assignment.  If automatic assignment is requested
%% 	TEI should be <tt>none</tt>.
%% 	Valid values are in the range <tt>0..63</tt> or <tt>127</tt>.
%% 	If the group TEI value <tt>127</tt> is specified
%% 	a broadcast DLE will be started, otherwise a point-to-point DLE
%% 	and CME are started.</p>
%%
%% 	<p>Possible options are:</p>
%% 	<dl>
%% 		<dt><tt>{role, Role}</tt></dt> <dd><tt>Role</tt> may be one of 
%% 				<tt>user</tt>, <tt>network</tt> or <tt>symmetrical</tt>.
%% 				Default is <tt>user</tt>.</dd>
%% 	</dl>
%%
%% 	<p>Returns <tt>{LME, CME, DLE}</tt> where <tt>LME</tt> is the pid
%% 	of the layer management entity which was created by a previous call 
%% 	to <a href="#start_link-3"><tt>lapd:start_link/3</tt></a>.  
%% 	<tt>CME</tt> is the pid of the
%% 	new connection management entity if a point-to-point DLE was started 
%% 	and undefined if a broadcast DLE was started.  <tt>DLE</tt> is the
%% 	pid of the new data link entity.  The <tt>DLE</tt> is used by 
%% 	the LAPD-User as the service access point for the LAPD service.</p>
%%
open(Sup, SAPI, TEI, Options) ->
	Children = supervisor:which_children(Sup),
	{value, {lme, LME, _, _}} = lists:keysearch(lme, 1, Children),
	{value, {sap, SUP, _, _}} = lists:keysearch(sap, 1, Children),
	gen_server:call(LME, {'SMAP', 'OPEN', request, {SUP, SAPI, TEI, Options}}).
	

%% @spec (LME::pid(), DLE::pid(), USAP::pid()) -> ok
%%
%% @doc Binds an open LAPD service access point to a LAPD-User.
%% 	<p><tt>LME</tt> and <tt>DLE</tt> are the pid of the layer 
%% 	management entity and data link entity returned from a previous
%% 	call to
%% 	<a href="#open-4"><tt>lapd:open/4</tt></a>.</p>
%%
%% 	<p><tt>USAP</tt> is the pid of the LAPD-User process.</p>
%%
bind(LME, DLE, USAP) ->
	gen_server:call(LME, {'SMAP', 'BIND', request, {DLE, USAP}}).


%% @spec (DLE::pid()) -> ok
%%
%% @doc Closes an open LAPD service access point.
%% 	<p><tt>DLE</tt> is the pid of the data link entity returned
%% 	from a previous call to
%% 	<a href="#open-4"><tt>lapd:open/4</tt></a>.</p>
%%
close(DLE) ->
	exit(DLE, shutdown).

