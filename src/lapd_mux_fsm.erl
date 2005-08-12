%%% $Id$
%%%---------------------------------------------------------------------
%%% @copyright 2004,2005 Motivity Telecom Inc.
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
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%%
%%% @doc A behaviour module for implementing the Q.921 LAPD multiplex procedures.
%%% 	<p>This module encapsulates the functionality required of the multiplex
%%% 	procedures.  Adaptations to specific layer 1 implementations may be 
%%% 	implemented using this module.  This module behaves to gen_fsm.</p>
%%%
%%% 	<p>The pupose of the multiplex procedures are to distribute frames
%%% 	received from layer 1 to the correct data link entity (DLE).  The 
%%% 	<tt>lapd_mux_fsm</tt> behaviour module handles this distribution
%%% 	function by maintaining a table of DLCIs for point-to-point and
%%% 	broadcast DLEs which stores those pids.  The <tt>lapd_mux_fsm</tt>
%%% 	behaviour module will handle L1 &lt;- L2 primitives.  The callback
%%% 	module must convert the implementation specific layer 1 API to the 
%%% 	primitive form expected.  The callback module must also accept the 
%%% 	primitive form of L1 &lt;- L2 and convert to the layer 1 
%%% 	implementation specific API.</p>
%%% 	
%%% 	<h3>Usage</h3>
%%% 	<p>The callback module should be implemented as a gen_fsm behaviour
%%% 	but with a <tt>lapd_mux_fsm</tt> behaviour module attribute:</p>
%%% 	<pre>-behaviour(lapd_mux_fsm).</pre>
%%%
%%% 	<p>The call back module must handle the L1 &lt;- L2 primitives
%%% 	in it's state(s):</p>
%%% 	<pre>StateName({'PH', 'ACTIVATE', request, _}, StateData) -&gt;</pre>
%%% 	<p>In the above state handler the callback module will enable the
%%% 	layer 1 implementation using the implementation specific API.</p>
%%%
%%% 	<pre>StateName({'PH', 'DATA', request, PDU}, StateData) when is_binary(PDU) -&gt;</pre>
%%% 	<p>In the above state handler the callback module will forward the 
%%% 	PDU to the layer 1 implementation using the implementation specific
%%% 	API.</p>
%%%
%%% 	<p>The callback module must convert the implementation specific API
%%% 	for received frames to send a primitive event for handling by the
%%% 	<tt>lapd_mux_fsm</tt> behaviour module:</p>
%%%
%%% 	<pre>handle_info({_Port, {'L3L4m', _CtrlBin, _DataBin}}, StateName, StateData) -&gt;
%%% 	     gen_fsm:send_event(self(), {'PH', 'DATA', indication, DataBin}),
%%% 	     {next_state, StateName, StateData}.
%%% 	</pre>
%%% 	
%%%
%%% @reference ITU-T Q.921 ISDN user-network interface - Data link layer specification 
%%%
%%% @reference ETSI ETS 300 125 Integrated Services Digital Network (ISDN);
%%% 	User-network interface data link layer specification;
%%% 	Application of CCITT Recommendations Q.920/I.440 and Q.921/I.441 
%%%
-module(lapd_mux_fsm).
-copyright('Copyright (c) 2004,2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision$').
-behaviour(gen_fsm).

% behaviour modules must export this function
-export([behaviour_info/1]).

% export the gen_fsm interface
-export([start/3, start/4, start_link/3, start_link/4,
		send_event/2, sync_send_event/2, sync_send_event/3,
		send_all_state_event/2, sync_send_all_state_event/2,
		sync_send_all_state_event/3, reply/2, send_event_after/2,
		start_timer/2, cancel_timer/1]).

% export the gen_fsm state handler call backs
-export([statename/2, statename/3]).

% export the gen_fsm common call backs
-export([init/1, handle_event/3, handle_sync_event/4,
		handle_info/3, terminate/3, code_change/4]).

%%
%% define what callbacks users must export
%%
%% @hidden
behaviour_info(callbacks) ->
	gen_fsm:behaviour_info(callbacks);
behaviour_info(Other) -> 
	gen_fsm:behaviour_info(Other).

-record(lapd_mux_state, {module, statename, lme, endpoints, statedata}).


%%----------------------------------------------------------------------
%%  The gen_fsm API functions
%%----------------------------------------------------------------------

%% @hidden
start(Module, Args, Options) ->
	gen_fsm:start(?MODULE, [Module, Args], Options).

%% @hidden
start(FsmRef, Module, Args, Options) ->
	gen_fsm:start(FsmRef, ?MODULE, [Module, Args], Options).

%% @hidden
start_link(Module, Args, Options) ->
	gen_fsm:start_link(?MODULE, [Module, Args], Options).

%% @hidden
start_link(FsmRef, Module, Args, Options) ->
	gen_fsm:start_link(FsmRef, ?MODULE, [Module, Args], Options).

%% @hidden
send_event(FsmRef, Event) ->
	gen_fsm:send_event(FsmRef, Event).

%% @hidden
sync_send_event(FsmRef, Event) ->
	gen_fsm:sync_send_event(FsmRef, Event).

%% @hidden
sync_send_event(FsmRef, Event, Timeout) ->
	gen_fsm:sync_send_event(FsmRef, Event, Timeout).

%% @hidden
send_all_state_event(FsmRef, Event) ->
	gen_fsm:send_all_state_event(FsmRef, Event).

%% @hidden
sync_send_all_state_event(FsmRef, Event) ->
	gen_fsm:sync_send_all_state_event(FsmRef, Event).

%% @hidden
sync_send_all_state_event(FsmRef, Event, Timeout) ->
	gen_fsm:sync_send_all_state_event(FsmRef, Event, Timeout).

%% @hidden
reply(Caller, Reply) ->
	gen_fsm:reply(Caller, Reply).

%% @hidden
send_event_after(Time, Event) ->
	gen_fsm:send_event_after(Time, Event).

%% @hidden
start_timer(Time, Msg) ->
	gen_fsm:start_timer(Time, Msg).

%% @hidden
cancel_timer(Ref) ->
	gen_fsm:cancel_timer(Ref).


%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% @hidden
init([Module, Args]) ->
	process_flag(trap_exit, true),
	case Module:init(Args) of
		{ok, StateName, StateData} ->
			State = #lapd_mux_state{module = Module, statename = StateName, 
					endpoints = gb_trees:empty(), statedata = StateData},
			{ok, statename, State};
		{ok, StateName, StateData, Timeout} ->
			State = #lapd_mux_state{module = Module, statename = StateName, 
					endpoints = gb_trees:empty(), statedata = StateData},
			{ok, statename, State, Timeout};
		{stop, Reason} ->
			{stop, Reason};
		ignore ->
			ignore;
		Other ->
			Other
	end.
                
%% @hidden
%% L1 -> M PDU L2 management
statename({'PH', 'DATA', indication, <<63:6, _:2, 127:7, _:1,  _/binary>>} = Event, State) ->
	{value, LME} = gb_trees:lookup({63, 127}, State#lapd_mux_state.endpoints),
	gen_server:cast(LME, Event),
	{next_state, statename, State};
%% L1 -> L2 PDU 
statename({'PH', 'DATA', indication, <<SAPI:6, _:2, TEI:7, _:1, _/binary>>} = Event, State) ->
	case gb_trees:lookup({SAPI, TEI}, State#lapd_mux_state.endpoints) of
		{value, DLE} ->
			gen_fsm:send_event(DLE, Event),
			{next_state, statename, State};
		none ->
			{next_state, statename, State}
	end;
%% L1 -> M L2 management
statename({'PH', 'ACTIVATE', indication, _}, State) ->
	{next_state, statename, State};
statename({'PH', 'DEACTIVATE', indication, _}, State) ->
	% TODO:  resend to all DLEs
	{next_state, statename, State};
statename(Event, State) ->
	Module = State#lapd_mux_state.module,
	StateName = State#lapd_mux_state.statename,
	case Module:StateName(Event, State#lapd_mux_state.statedata) of
		{next_state, NextStateName, NewStateData} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState};
		{next_state, NextStateName, NewStateData, Timeout} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState, Timeout};
		{stop, Reason, NewStateData} ->
			NewState = State#lapd_mux_state{statedata = NewStateData},
			{stop, Reason, NewState};
		Other ->
			Other
	end.

%% @hidden
statename(Event, From, State) ->
	Module = State#lapd_mux_state.module,
	StateName = State#lapd_mux_state.statename,
	case Module:StateName(Event, From, State#lapd_mux_state.statedata) of
		{reply, Reply, NextStateName, NewStateData} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{reply, Reply, statename, NewState};
		{reply, Reply, NextStateName, NewStateData, Timeout} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{reply, Reply, statename, NewState, Timeout};
		{next_state, NextStateName, NewStateData} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState};
		{next_state, NextStateName, NewStateData, Timeout} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState, Timeout};
		{stop, Reason, Reply, NewStateData} ->
			NewState = State#lapd_mux_state{statedata = NewStateData},
			{stop, Reason, Reply, NewState};
		{stop, Reason, NewStateData} ->
			NewState = State#lapd_mux_state{statedata = NewStateData},
			{stop, Reason, NewState};
		Other ->
			Other
	end.

%% @hidden
%% data link connection endpoint assigned
handle_event({open, {SAPI, TEI, DLE}}, statename, State) ->
	NewEndPoints = gb_trees:insert({SAPI, TEI}, DLE, State#lapd_mux_state.endpoints),
	NewState = State#lapd_mux_state{endpoints = NewEndPoints},
	{next_state, statename, NewState};
%% data link connection endpoint closed
handle_event({close, {SAPI, TEI, _DLE}}, statename, State) ->
	NewEndPoints = gb_trees:delete_any({SAPI, TEI}, State#lapd_mux_state.endpoints),
	NewState = State#lapd_mux_state{endpoints = NewEndPoints},
	{next_state, statename, NewState};
handle_event(Event, statename, State) ->
	Module = State#lapd_mux_state.module,
	case Module:handle_event(Event, State#lapd_mux_state.statedata) of
		{next_state, NextStateName, NewStateData} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState};
		{next_state, NextStateName, NewStateData, Timeout} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState, Timeout};
		{stop, Reason, NewStateData} ->
			NewState = State#lapd_mux_state{statedata = NewStateData},
			{stop, Reason, NewState};
		Other ->
			Other
	end.

%% @hidden
handle_sync_event(Event, From, statename, State) ->
	Module = State#lapd_mux_state.module,
	StateName = State#lapd_mux_state.statename,
	case Module:handle_sync_event(Event, From, StateName, State#lapd_mux_state.statedata) of
		{reply, Reply, NextStateName, NewStateData} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{reply, Reply, statename, NewState};
		{reply, Reply, NextStateName, NewStateData, Timeout} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{reply, Reply, statename, NewState, Timeout};
		{next_state, NextStateName, NewStateData} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState};
		{next_state, NextStateName, NewStateData, Timeout} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState, Timeout};
		{stop, Reason, Reply, NewStateData} ->
			NewState = State#lapd_mux_state{statedata = NewStateData},
			{stop, Reason, Reply, NewState};
		{stop, Reason, NewStateData} ->
			NewState = State#lapd_mux_state{statedata = NewStateData},
			{stop, Reason, NewState};
		Other ->
			Other
	end.

%% @hidden
handle_info(Event, statename, State) ->
	Module = State#lapd_mux_state.module,
	StateName = State#lapd_mux_state.statename,
	case Module:handle_info(Event, StateName, State#lapd_mux_state.statedata) of
		{next_state, NextStateName, NewStateData} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState};
		{next_state, NextStateName, NewStateData, Timeout} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{next_state, statename, NewState, Timeout};
		{stop, Reason, NewStateData} ->
			NewState = State#lapd_mux_state{statedata = NewStateData},
			{stop, Reason, NewState};
		Other ->
			Other
	end.

%% @hidden
terminate(Reason, statename, State) ->
	Module = State#lapd_mux_state.module,
	StateName = State#lapd_mux_state.statename,
	StateData = State#lapd_mux_state.statedata,
	Module:terminate(Reason, StateName, StateData).

%% @hidden
code_change(OldVersion, statename, State, Extra) ->
	Module = State#lapd_mux_state.module,
	StateName = State#lapd_mux_state.statename,
	StateData = State#lapd_mux_state.statedata,
	case Module:code_change(OldVersion, StateName, StateData, Extra) of
		{ok, NextStateName, NewStateData} ->
			NewState = State#lapd_mux_state{statename = NextStateName, statedata = NewStateData},
			{ok, statename, NewState};
		Other ->
			Other
	end.

