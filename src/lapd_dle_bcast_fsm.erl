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
%%% @doc A finite state machine implementing a Q.921 LAPD data link entity.
%%% 	<p>An implementation of the broadcast datalink procedures defined
%%% 	in Q.921 link access procedures for the D-channel.<p>
%%%
%%% @reference ITU-T Q.921 ISDN user-network interface - Data link layer specification 
%%%
%%% @reference ETSI ETS 300 125 Integrated Services Digital Network (ISDN);
%%% 	User-network interface data link layer specification;
%%% 	Application of CCITT Recommendations Q.920/I.440 and Q.921/I.441 
%%%
%%% @private
         
-module(lapd_dle_bcast_fsm).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(gen_fsm).

-export([init/1, terminate/3]).
-export([information_transfer/2]).
-export([handle_event/3, handle_info/3, handle_sync_event/4, code_change/4]).

%% MUX:  Multiplex Procedures (Layer 1)
%% USAP: User Service Access Point (Layer 3)
%% SAPI: Service Access Point Identifier
-record(state, {mux, sapi, usap).

init([MUX, SAPI, Options]) ->
	Role = case lists:keysearch(role, 1, Options) of
		{value, Value} -> Value;
		_ -> user
	end,
	process_flag(trap_exit, true),
	StateData = #state{mux = MUX, sapi = SAPI, role = Role},
	{ok, await_bind, StateData}.

%% ref:  ETS 300 125 Figure C-1/Q.921
information_transfer({_, 'UNIT DATA', request, PDU}, StateData) when is_binary(Data) ->
	case StateData#state.role of
		network -> CR = 1;
		_ -> CR = 0
	end,
	% P=0
	UI = <<(StateData#state.sapi):6, CR:1, 0:1, 127:7, 1:1,
			2#000:3, 0:1, 2#00:2, 2#11:2, Data/binary>>,
	% TX UI
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UI}),
	{next_state, information_transfer, StateData};
information_transfer({'PH', 'DATA', indication, 
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, _P:1, 2#00:2, 2#11:2,     % Command (UI)
		Data/binary>>},                    % Information
		StateData) ->
	% DL UNIT DATA indication
	case StateData#state.sapi of
		63 ->
			% M <- L2
			catch gen_fsm:send_event(StateData#state.usap, {'MDL', 'UNIT DATA', indication, Data}),
			{next_state, tei_assigned, StateData};
		_ ->
			% L3 <- L2
			catch gen_fsm:send_event(StateData#state.usap, {'DL', 'UNIT DATA', indication, Data}),
			{next_state, tei_assigned, StateData}
	end;
information_transfer({'PH', 'DEACTIVATE', indication, _PhParms}, StateData) ->
	% Discard UI queue
	{next_state, information_transfer, StateData};
information_transfer(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, information_transfer}, Event]),
	{next_state, information_transfer, StateData}.

%% implementation specific state and primitive for associating a LAPD-User pid()
handle_event({'MDL', 'BIND', request, USAP}, StateName, StateData) ->
	{next_state, StateName, StateData#state{usap = USAP}};
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, StateData) ->
	gen_fsm:send_all_state_event(StateData#state.mux, {close, {bcast, StateData#state.sapi, self()}}).

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------

