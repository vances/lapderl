%%%---------------------------------------------------------------------
%%% @copyright Motivity Telecom Inc. 2004
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
%%% @doc A behaviour module for implementing the Q.921 LAPD multiplex procedures.
%%% 	<p>This module encapsulates the functionality required of the multiplex
%%% 	procedures.  Adaptations to specific layer 1 implementations may be 
%%% 	implemented using this module.  This module behaves to gen_fsm.</p>
%%%
%%% @reference ITU-T Q.921 ISDN user-network interface - Data link layer specification 
%%%
%%% @reference ETSI ETS 300 125 Integrated Services Digital Network (ISDN);
%%% 	User-network interface data link layer specification;
%%% 	Application of CCITT Recommendations Q.920/I.440 and Q.921/I.441 
%%%
         
-module(lapd_mux_fsm).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
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
%% define what call backs users must export
%%
behaviour_info(callbacks) ->
	gen_fsm:behaviour_info(callbacks);
behaviour_info(Other) -> 
	gen_fsm:behaviour_info(Other).

-record(lapd_mux_state, {module, statename, lme, p2p_sapis, bcast_sapis, statedata}).


%%----------------------------------------------------------------------
%%  The gen_fsm API functions
%%----------------------------------------------------------------------

start(Module, Args, Options) ->
	gen_fsm:start(?MODULE, [Module, Args], Options).

start(FsmRef, Module, Args, Options) ->
	gen_fsm:start(FsmRef, ?MODULE, [Module, Args], Options).

start_link(Module, Args, Options) ->
	gen_fsm:start_link(?MODULE, [Module, Args], Options).

start_link(FsmRef, Module, Args, Options) ->
	gen_fsm:start_link(FsmRef, ?MODULE, [Module, Args], Options).

send_event(FsmRef, Event) ->
	gen_fsm:send_event(FsmRef, Event).

sync_send_event(FsmRef, Event) ->
	gen_fsm:sync_send_event(FsmRef, Event).

sync_send_event(FsmRef, Event, Timeout) ->
	gen_fsm:sync_send_event(FsmRef, Event, Timeout).

send_all_state_event(FsmRef, Event) ->
	gen_fsm:send_all_state_event(FsmRef, Event).

sync_send_all_state_event(FsmRef, Event) ->
	gen_fsm:sync_send_all_state_event(FsmRef, Event).

sync_send_all_state_event(FsmRef, Event, Timeout) ->
	gen_fsm:sync_send_all_state_event(FsmRef, Event, Timeout).

reply(Caller, Reply) ->
	gen_fsm:reply(Caller, Reply).

send_event_after(Time, Event) ->
	gen_fsm:send_event_after(Time, Event).

start_timer(Time, Msg) ->
	gen_fsm:start_timer(Time, Msg).

cancel_timer(Ref) ->
	gen_fsm:cancel_timer(Ref).


%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

init([Module, Args]) ->
	process_flag(trap_exit, true),
	case Module:init(Args) of
		{ok, StateName, StateData} ->
			State = #lapd_mux_state{module = Module, statename = StateName, 
					p2p_sapis = gb_trees:empty(),
					bcast_sapis = gb_trees:empty(),
					statedata = StateData},
			{ok, statename, State};
		{ok, StateName, StateData, Timeout} ->
			State = #lapd_mux_state{module = Module, statename = StateName, 
					p2p_sapis = gb_trees:empty(),
					bcast_sapis = gb_trees:empty(),
					statedata = StateData},
			{ok, statename, State, Timeout};
		{stop, Reason} ->
			{stop, Reason};
		ignore ->
			ignore;
		Other ->
			Other
	end.
                
%% L1 -> L2 PDU (Broadcast Datalink Procedures) L2 management
statename({'PH', 'DATA', indication, <<_:2, 63:6, _:1, 127:7, _/binary>>} = Event, State) ->
	gen_server:cast(State#lapd_mux_state.lme, Event),
	{next_state, statename, State};
%% L1 -> L2 PDU (Broadcast Datalink Procedures)
statename({'PH', 'DATA', indication, <<_:2, SAPI:6, _:1, 127:7, _/binary>>} = Event, State) ->
	case gb_trees:lookup(SAPI, State#lapd_mux_state.bcast_sapis) of
		{value, DLE} ->
			gen_fsm:send_event(DLE, Event),
			{next_state, statename, State};
		none ->
			{next_state, statename, State}
	end;
%% L1 -> L2 PDU (Point to Point Data Link Procedures)
statename({'PH', 'DATA', indication, <<_:2, SAPI:6, _/binary>>} = Event, State) ->
	case gb_trees:lookup(SAPI, State#lapd_mux_state.p2p_sapis) of
		{value, DLE} ->
			gen_fsm:send_event(DLE, Event),
			{next_state, statename, State};
		none ->
			{next_state, statename, State}
	end;
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

%% post initialization assignment of LME pid
handle_event({lme, LME}, statename, State) ->
	NewState = State#lapd_mux_state{lme = LME},
	{next_state, statename, NewState};
%% point-to-point sapi opened
handle_event({open, {p2p, SAPI, DLE}}, statename, State) ->
	NewSapis = gb_trees:insert(SAPI, DLE, State#lapd_mux_state.p2p_sapis),
	NewState = State#lapd_mux_state{p2p_sapis = NewSapis},
	{next_state, statename, NewState};
%% broadcast sapi opened
handle_event({open, {bcast, SAPI, DLE}}, statename, State) ->
	NewSapis = gb_trees:insert(SAPI, DLE, State#lapd_mux_state.bcast_sapis),
	NewState = State#lapd_mux_state{bcast_sapis = NewSapis},
	{next_state, statename, NewState};
%% point-to-point sapi closed
handle_event({close, {p2p, SAPI, _DLE}}, statename, State) ->
	NewSapis = gb_trees:delete_any(SAPI, State#lapd_mux_state.p2p_sapis),
	NewState = State#lapd_mux_state{p2p_sapis = NewSapis},
	{next_state, statename, NewState};
%% broadcast sapi closed
handle_event({close, {broadcast, SAPI, _DLE}}, statename, State) ->
	NewSapis = gb_trees:delete_any(SAPI, State#lapd_mux_state.bcast_sapis),
	NewState = State#lapd_mux_state{bcast_sapis = NewSapis},
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

handle_info(Event, statename, State) ->
	Module = State#lapd_mux_state.module,
	case Module:handle_info(Event, State#lapd_mux_state.statedata) of
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

terminate(Reason, statename, State) ->
	Module = State#lapd_mux_state.module,
	StateName = State#lapd_mux_state.statename,
	StateData = State#lapd_mux_state.statedata,
	Module:terminate(Reason, StateName, StateData).

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

