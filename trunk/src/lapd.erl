%%%---------------------------------------------------------------------
%%% @copyright 2004,2005 Motivity Telecom Inc. 2004
%%% @end
%%%
%%% Copyright (c) 2004,2005 Motivity Telecom Inc.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%       - Redistributions of source code must retain the above
%%%         copyright notice, this list of conditions and the following
%%%         disclaimer.
%%%       - Redistributions in binary form must reproduce the above
%%%         copyright notice, this list of conditions and the following
%%%         disclaimer in the documentation and/or other materials 
%%%         provided with the distribution.
%%%       - Neither the name of Motivity Telecom Inc. nor the names of
%%%         its contributors may be used to endorse or promote products
%%%         derived from this software without specific prior written
%%%         permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
%%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
%%% POSSIBILITY OF SUCH DAMAGE.
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
-copyright('Copyright (c) 2004,2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').

%% our published API functions
-export([start_link/3, stop/1]).
-export([open/4, close/1]).
-export([bind/3]).


%% @type lapd_options(). LAPD layer options
%% 	<p>A list of one or more of the following tuples.</p>
%% 	<dl>
%%			<dt><tt>{role, Role}</tt></dt><dd><tt>user</tt> |
%% 				<tt>network</tt> default is <tt>user</tt></dd>
%%			<dt><tt>{k, Value}</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>{n200, Value}</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>{n201, Value}</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>{n202, Value}</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>{t200, Value}</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>{t201, Value}</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>{t202, Value}</tt></dt><dd><tt>integer()</tt></dd>
%%			<dt><tt>{t203, Value}</tt></dt><dd><tt>integer()</tt></dd>
%% 	</dl>
%% @end

%%----------------------------------------------------------------------
%%  The API functions
%%----------------------------------------------------------------------

%% @spec (Module::atom(), Args::term(), Options::term()) -> Result
%% 	Options = [lapd_options() | GenFsmOptions]
%% 	GenFsmOptions = [tuple()]
%% 	Result = {ok, Sup} | {error, Reason}
%% 	Sup = pid()
%%		Reason = term()
%%
%% @doc Start a new LAPD layer.
%% 	<p>This function creates a supervision tree for the new LAPD 
%% 	layer.  A layer management entity (LME) process and a multiplexer 
%% 	process are started along with a broadcast data link entity
%% 	(SAPI 63, TEI 127) for use by the LME.</p>
%%
%% 	<p><tt>Module</tt> is the name of a <a href="lapd_mux_fsm.html">
%% 	<tt>lapd_mux_fsm</tt></a> behaviour callback module which will
%% 	provide the layer 1 adaptation.</p>
%%
%% 	<p><tt>Args</tt> has meaning only to the callback module but 
%% 	presumably would identify the specific layer 1 service access 
%% 	point for this LAPD layer.</p>
%%
%% 	<p><tt>Options</tt> may include lapd_options() and gen_fsm
%% 	options passed to the layer 1 adaptation callback module.</p>
%%
%% 	<p>The callback module will be started with:<br/>
%% 		<tt>gen_fsm:start_link(Module, Args, GenFsmOptions)</tt>
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
			{value, {sap, SapSup, _, _}} = lists:keysearch(sap, 1, Children),
			gen_server:call(LME, {activate, SapSup, MUX}),
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


%% @spec (Sup::pid(), SAPI::integer(), TEI::integer(), Options::lapd_options()) -> {LME, CME, DLE}
%% 	LME = pid()
%% 	CME = pid()
%% 	DLE = pid()
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

