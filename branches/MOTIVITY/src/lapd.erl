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
         
-module(lapd).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').

%% our published API functions
-export([start_link/1, stop/1]).
-export([open/3, close/2]).
-export([bind/3]).


%%----------------------------------------------------------------------
%%  The API functions
%%----------------------------------------------------------------------

%% @spec (PhySAP::pid()) -> Result
%% 	Result = {ok, Pid} | {error, Reason}
%% 	Pid = pid()
%%		Reason = term()
%%
%% @doc Start a new LAPD layer.
%% 	<p>Starts a supervision tree for the new LAPD layer as well as a
%% 	layer management entity process.</p>
%% 	<p>Returns <tt>{ok, Pid}</tt> or <tt>{error, Reason}</tt>.  This
%% 	function is intended to be used as the start function of a
%% 	child specification in an application's supervision tree.</p>
%%
start_link(PhySAP) ->
	supervisor:start_link(lapd_sup, [PhySAP]).


%% @spec (Sup::pid()) -> true
%%
%% @doc Stop a running LAPD layer.
%% 	<p>Terminates the top level supervisor for the layer which in turn
%% 	stops all the running processes in the layer.</p>
%%
stop(Sup) ->
	exit(Sup, shutdown).


%% @spec (Sup::pid(), SAPI::integer(), Options::option_list()) -> {LME, CME, DLE}
%% 	CME = pid()
%% 	DLE = pid()
%%
%% @type option_list() = [term()].
%%
%% @doc Open a LAPD service access point.
%% 	<p>Creates a new data link entity and associated connection 
%% 	management entity process.</p>
%% 	<p><tt>Sup</tt> is the pid() of the top level supervisor for a
%% 	layer returned from a previous call to lapd:start_link/1.</p>
%% 	<p><tt>SAPI</tt> specifies the service access point identifier.
%% 	Valid values are in the range 0..63.</p>
%% 	<p>Some standardized SAPI values are:</p>
%% 	<dl>
%% 		<dt><tt>0</tt></dt> <dd>Call control procedures</dd>
%% 		<dt><tt>16</tt></dt> <dd>Packet communication conforming to
%% 				X.25 level 3 procedures</dd>
%% 		<dt><tt>63</tt></dt> <dd>Layer 2 management procedures</dd>
%% 	</dl>
%% 	<p>Possible options are:</p>
%% 	<dl>
%% 		<dt><tt>{role, Role}</tt></dt> <dd><tt>Role</tt> may be one of 
%% 				<tt>user</tt>, <tt>network</tt> or <tt>symmetrical</tt>.
%% 				Default is <tt>user</tt>.</dd>
%% 	</dl>
%% 	<p>Returns <tt>{LME, CME, DLE}</tt> where <tt>LME</tt> is the pid
%% 	of the layer management entity which was created by a previous call 
%% 	to <tt>lapd:start_link/1</tt>, <tt>CME</tt> is the pid of the
%% 	new connection management entity and <tt>DLE</tt> is the pid of the
%% 	the pid of the new data link entity.  The <tt>DLE</tt> is used by 
%% 	the LAPD-User as the service access point for the LAPD service.</p>
%%
open(Sup, SAPI, Options) ->
	Children = supervisor:which_children(Sup),
	{value, {lme, LME, _, _}} = lists:keysearch(lme, 1, Children),
	{value, {sap, SUP, _, _}} = lists:keysearch(sap, 1, Children),
	gen_server:call(LME, {'SMAP', 'OPEN', request, {SUP, SAPI, Options}}).
	

%% @spec (LME::pid(), DLE::pid(), USAP::pid()) -> ok
%%
%% @doc Binds an open LAPD service access point to a LAPD-User.
%% 	<p><tt>LME</tt> and <tt>DLE</tt> are the pid of the layer 
%% 	management entity and data link entity returned from a previous
%% 	call to <tt>lapd:open/3</tt>.</p>
%% 	<p><tt>USAP</tt> is the pid of the LAPD-User process.</p>
%%
bind(LME, DLE, USAP) ->
	gen_server:call(LME, {'SMAP', 'BIND', request, {DLE, USAP}}).


%% @spec (LME::pid(), DLE::pid()) -> ok
%%
%% @doc Closes an open LAPD service access point.
%% 	<p><tt>LME</tt> and <tt>DLE</tt> are the pid of the layer 
%% 	management entity and data link entity returned from a previous
%% 	call to <tt>lapd:open/3</tt>.</p>
%%
close(LME, DLE) ->
	gen_server:call(LME, {'SMAP', 'CLOSE', request, DLE}).

