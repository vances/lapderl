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
%%% 	<p>An implementation of the point-to-point datalink procedures
%%% 	defined in Q.921 link access procedures for the D-channel.<p>
%%%
%%% @reference ITU-T Q.921 ISDN user-network interface - Data link layer specification 
%%%
%%% @reference ETSI ETS 300 125 Integrated Services Digital Network (ISDN);
%%% 	User-network interface data link layer specification;
%%% 	Application of CCITT Recommendations Q.920/I.440 and Q.921/I.441 
%%%
%%% @private
         
-module(lapd_dle_p2p_fsm).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(gen_fsm).

-export([init/1, terminate/3]).
-export([await_cme/2]).
-export([tei_unassigned/2, assign_awaiting_tei/2, establish_awaiting_tei/2,
		tei_assigned/2, awaiting_establishment/2, awaiting_release/2,
		multiple_frame_established/2, timer_recovery/2]).
-export([handle_event/3, handle_info/3, handle_sync_event/4, code_change/4]).

%% MUX:  Multiplex Procedures (Layer 1)
%% USAP: User Service Access Point (Layer 3)
%% LME:  Layer Management Entity
%% CME:  Connection Management Entity
%% SAPI: Service Access Point Identifier
-record(state, {mux, lme, cme, tei, sapi, role, usap,
		% MDL-ERROR codes
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
		% ref: ETS 300 125 B.4 The use of queues
		ui_queue = [], i_queue = [],
		% ref: ETS 300 125 3.5.2 Multiple frame operation 
		%		 - variables and sequence numbers
		'V(S)', 'V(A)', 'V(R)',
		% ref: ETS 300 125 5.9 List of system parameters
		n200 = 3, n201 = 260, n202 = 3, k = 7,
		t200 = 1000, t201 = 1000, t202 = 2000, t203 = 10000,
		t200_ref, t201_ref, t202_ref, t203_ref,
		% counters
		rc = 0,
		% variables from SDL
		establishable, layer3_initiated, own_receiver_busy, acknowledge_pending,
		peer_receiver_busy, reject_exception}).

init([PhySAP, SAPI, LME, Options]) ->
	Role = case lists:keysearch(role, 1, Options) of
		{value, network} -> network;
		_ -> user
	end,
	process_flag(trap_exit, true),
	StateData = #state{mux = PhySAP, lme = LME, sapi = SAPI, role = Role},
	{ok, await_cme, StateData}.

await_cme({cme, CME}, StateData) ->
	{next_state, tei_unassigned, StateData#state{cme = CME}}.

%% ref:  ETS 300 125 Figure B-3/Q.921 (1 of 3) 
tei_unassigned({'DL', 'ESTABLISH', request, _DlParms}, StateData) ->
	gen_fsm:send_event(StateData#state.lme,
			{'MDL', 'ASSIGN', indication, {undefined, self()}}),
	{next_state, establish_awaiting_tei, StateData};
tei_unassigned({'DL', 'UNIT DATA', request, Data}, StateData) when is_binary(Data) ->
	gen_fsm:send_event(StateData#state.lme,
			{'MDL', 'ASSIGN', indication, {undefined, self()}}),
	% UNIT DATA into UI queue
	NewStateData = StateData#state{ui_queue = StateData#state.ui_queue ++ [Data]},
	{next_state, assign_awaiting_tei, NewStateData};
tei_unassigned({'MDL', 'ASSIGN', request, {TEI, CES}}, StateData) 
		when CES == self() ->
	% Save TEI
	NewStateData = transmit_uiqueue(StateData#state{tei = TEI}),
	{next_state, tei_assigned, NewStateData};
tei_unassigned(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, tei_unassigned}, Event]),
	{next_state, tei_unassigned, StateData}.


%% ref:  ETS 300 125 Figure B-3/Q.921 (2 of 3) 
assign_awaiting_tei({'DL', 'ESTABLISH', request, _DlParms}, StateData) ->
	{next_state, establish_awaiting_tei, StateData};
assign_awaiting_tei({'DL', 'UNIT DATA', request, Data}, StateData) when is_binary(Data) ->
	% UNIT DATA into UI queue
	NewStateData = StateData#state{ui_queue = StateData#state.ui_queue ++ [Data]},
	% UI Frame queued up
	{next_state, assign_awaiting_tei, NewStateData};
assign_awaiting_tei({'MDL', 'ASSIGN', request, {TEI, _CES}}, StateData) ->
	% Save TEI
	NewStateData = transmit_uiqueue(StateData#state{tei = TEI}),
	{next_state, tei_assigned, NewStateData};
assign_awaiting_tei({'MDL', 'ERROR', response, _Reason}, StateData) ->
	% Discard UI queue
	NewStateData = StateData#state{ui_queue = []},
	{next_state, tei_unassigned, NewStateData};
assign_awaiting_tei({'PH', 'DEACTIVATE', indication, _PhParms}, StateData) ->
	% Discard UI queue
	NewStateData = StateData#state{ui_queue = []},
	{next_state, tei_unassigned, NewStateData};
assign_awaiting_tei(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, assign_awaiting_tei}, Event]),
	{next_state, assign_awaiting_tei, StateData}.


%% ref:  ETS 300 125 Figure B-3/Q.921 (3 of 3) 
establish_awaiting_tei({'DL', 'UNIT DATA', request, Data}, StateData) when is_binary(Data) ->
	% UNIT DATA into UI queue
	NewStateData = StateData#state{ui_queue = StateData#state.ui_queue ++ [Data]},
	% UI Frame queued up
	{next_state, establish_awaiting_tei, NewStateData};
establish_awaiting_tei({'MDL', 'ASSIGN', request, {TEI, _CES}}, StateData) ->
	% Save TEI
	NextStateData = StateData#state{tei = TEI},
	% Establish data link
	NextStateData2 = establish_data_link(NextStateData),
	% Set layer 3 initiated
	NewStateData = transmit_uiqueue(NextStateData2#state{layer3_initiated = true}),
	{next_state, awaiting_establishment, NewStateData};
establish_awaiting_tei({'MDL', 'ERROR', response, _Reason}, StateData) ->
	% Discard UI queue
	NewStateData = StateData#state{ui_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(StateData#state.usap,
			{'DL', 'RELEASE', indication, undefined}),
	{next_state, tei_unassigned, NewStateData};
establish_awaiting_tei({'PH', 'DEACTIVATE', indication, _PhParms}, StateData) ->
	% Discard UI queue
	NewStateData = StateData#state{ui_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(StateData#state.usap,
			{'DL', 'RELEASE', indication, undefined}),
	{next_state, tei_unassigned, NewStateData};
establish_awaiting_tei(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, establish_awaiting_tei}, Event]),
	{next_state, establish_awaiting_tei, StateData}.


%% ref:  ETS 300 125 Figure B-4/Q.921 (1 of 2) 
tei_assigned({'DL', 'ESTABLISH', request, _DlParms}, StateData) ->
	% Establish data link
	NewStateData = establish_data_link(StateData),
	% Set layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = true}};
tei_assigned({'DL', 'RELEASE', request, _DlParms}, StateData) ->
	% DL RELEASE confirm
	catch gen_fsm:send_event(StateData#state.usap,
			{'DL', 'RELEASE', confirm, undefined}),
	{next_state, tei_assigned, StateData};
tei_assigned({'MDL', 'REMOVE', request, {_TEI, _CES}}, StateData) ->
	% Discard UI queue
	NewStateData = StateData#state{tei = undefined, ui_queue = []},
	{next_state, tei_unassigned, NewStateData};
tei_assigned({'PH', 'DEACTIVATE', indication, _PhParms}, StateData) ->
	% Discard UI queue
	NewStateData = StateData#state{ui_queue = []},
	{next_state, tei_assigned, NewStateData}; 
tei_assigned({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, _P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) when StateData#state.role == network, CR == 1 ->  % Network/User side mismatch
	{next_state, tei_assigned, StateData};
tei_assigned({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, _P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) when StateData#state.role == user , CR == 0 ->  % Network/User side mismatch
	{next_state, tei_assigned, StateData};
tei_assigned({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#011:3, P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) when StateData#state.establishable == false ->
	% able to establish? (no)
	DM = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#000:3, P:1, 2#11:2, 2#11:2>>,
	% TX DM
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, DM}),
	{next_state, tei_assigned, StateData};
tei_assigned({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#011:3, P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) ->
	% able to establish? (yes)
	UA = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#011:3, P:1, 2#00:2, 2#11:2>>,
	% TX UA
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UA}),
	% Clear exception conditions
	NextStateData = clear_exception_conditions(StateData),
	% DL ESTABLISH indication
	catch gen_fsm:send_event(NextStateData#state.usap, {'DL', 'ESTABLISH', indication, undefined}),
	% V(S)=0, V(A)=0, V(R)=0
	% Start T203
	cancel_timer(NextStateData#state.t203_ref),
	T203_ref = gen_fsm:send_event_after(NextStateData#state.t203, t203_expiry),
	NewStateData = NextStateData#state{'V(S)' = 0, 'V(A)' = 0, 'V(R)' = 0, t203_ref = T203_ref},
	{next_state, multiple_frame_established, NewStateData};
tei_assigned({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#010:3, P:1, 2#00:2, 2#11:2>>},   % Command (DISC)
		StateData) ->
	DM = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#000:3, P:1, 2#11:2, 2#11:2>>,
	% TX DM
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, DM}),
	{next_state, tei_assigned, StateData};
% ref:  ETS 300 125 Figure B-4/Q.921 (2 of 2) 
tei_assigned({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 1:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% F=1? (yes)
	% MDL ERROR indication (C)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'C'}),
	{next_state, tei_assigned, StateData};
tei_assigned({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 0:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% F=1? (no)
	% MDL ERROR indication (D)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'D'}),
	{next_state, tei_assigned, StateData};
tei_assigned({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 1:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (yes)
	{next_state, tei_assigned, StateData};
tei_assigned({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 0:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) when StateData#state.establishable == false ->
	% F=1? (no)
	% Able to establish? (no)
	{next_state, tei_assigned, StateData};
tei_assigned({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 0:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (no)
	% Able to establish? (yes)
	% Establish data link
	NewStateData = establish_data_link(StateData),
	% Set layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = true}};
%% ref:  ETS 300 125 Figure B-9/Q.921 (1 of 5) 
tei_assigned({'DL', 'UNIT DATA', request, Data}, StateData) when is_binary(Data) ->
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=0
	UI = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#000:3, 0:1, 2#00:2, 2#11:2, Data/binary>>,
	% TX UI
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UI}),
	{next_state, tei_assigned, StateData};
tei_assigned({'PH', 'DATA', indication, 
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, _P:1, 2#00:2, 2#11:2,     % Command (UI)
		Data/binary>>},                    % Information
		StateData) ->
	% DL UNIT DATA indication
	catch gen_fsm:send_event(StateData#state.usap, {'DL', 'UNIT DATA', indication, Data}),
	{next_state, tei_assigned, StateData};
% ref:  ETS 300 125 Figure B-9/Q.921 (3 of 5) 
% ref:  ETS 300 125 5.8.5 Frame rejection condition
tei_assigned({'PH', 'DATA', indication, _DlParms}, StateData) -> 
	% MDL ERROR indication (L)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'L'}),
	{next_state, tei_assigned, StateData};
%tei_assigned({'PH', 'DATA', indication, _DlParms}, StateData) -> 
%	% MDL ERROR indication (M,N,O)
%	gen_fsm:send_event(StateData#state.cme, 'M'),
%	gen_fsm:send_event(StateData#state.cme, 'N'),
%	gen_fsm:send_event(StateData#state.cme, 'O'),
%	{next_state, tei_assigned, StateData};
tei_assigned(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, tei_assigned}, Event]),
	{next_state, tei_assigned, StateData}.

%% ref:  ETS 300 125 Figure B-5/Q.921 (1 of 3) 
awaiting_establishment({'DL', 'ESTABLISH', request, _DlParms}, StateData) 
		% NOTE:  only possible in cases of leyer 2 initiated re-estabslishment
		when StateData#state.layer3_initiated /= true ->
	% Discard I queue
	% Set layer 3 initiated
	NewStateData = StateData#state{i_queue = [], layer3_initiated = true},
	{next_state, awaiting_establishment, NewStateData};
% TODO:  "Save a signal (until completion of a transition to a new state)"
% awaiting_establishment({'DL', 'RELEASE', request, _DlParms}, StateData) ->
awaiting_establishment({'MDL', 'REMOVE', request, {_TEI, _CES}}, StateData) ->
	% Discard I & UI queues
	NewStateData = StateData#state{tei = undefined, i_queue = [], ui_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(StateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	cancel_timer(NewStateData#state.t200_ref),
	{next_state, tei_unassigned, NewStateData#state{t200_ref = undefined}};
awaiting_establishment({'PH', 'DEACTIVATION', indication, _PhParms}, StateData) ->
	% Disacrd I & UI queues
	NewStateData = StateData#state{i_queue = [], ui_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(StateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	cancel_timer(NewStateData#state.t200_ref),
	{next_state, tei_assigned, NewStateData#state{t200_ref = undefined}};
awaiting_establishment({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, _P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) when StateData#state.role == network, CR == 1 ->  % Network/User side mismatch
	{next_state, awaiting_establishment, StateData};
awaiting_establishment({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, _P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) when StateData#state.role == user , CR == 0 ->  % Network/User side mismatch
	{next_state, awaiting_establishment, StateData};
awaiting_establishment({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#011:3, P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) ->
	UA = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#011:3, P:1, 2#00:2, 2#11:2>>,
	% F=P
	% TX UA
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UA}),
	{next_state, awaiting_establishment, StateData};
% ref:  ETS 300 125 Figure B-5/Q.921 (2 of 3) 
awaiting_establishment({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#010:3, P:1, 2#00:2, 2#11:2>>},   % Command (DISC)
		StateData) ->
	% F=P
	% TX UA
	UA = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#011:3, P:1, 2#00:2, 2#11:2>>,
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UA}),
	{next_state, awaiting_establishment, StateData};
% ref:  ETS 300 125 Figure B-4/Q.921 (2 of 2) 
awaiting_establishment({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 1:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) when StateData#state.layer3_initiated == true ->
	% F=1? (yes)
	% Layer 3 initiated? (yes)
	% DL ESTABLISH confirm
	catch gen_fsm:send_event(StateData#state.usap, {'DL', 'ESTABLISH', confirm, undefined}),
	% Stop T200
	cancel_timer(StateData#state.t200_ref),
	% Start T203
	cancel_timer(StateData#state.t203_ref),
	T203_ref = gen_fsm:send_event_after(StateData#state.t203, t203_expiry),
	% V(S)=0, V(A)=0, V(R)=0
	NewStateData = StateData#state{t200_ref = undefined, t203_ref = T203_ref,
			'V(S)' = 0, 'V(A)' = 0, 'V(R)' = 0},
	{next_state, multiple_frame_established, NewStateData};
awaiting_establishment({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 1:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% F=1? (yes)
	% Layer 3 initiated? (no)
	% V(S)=V(A)?
	VA = StateData#state.'V(A)',
	case StateData#state.'V(S)' of
		VA ->
			StateData;
		_ ->
			% DL ESTABLISH indication
			catch gen_fsm:send_event(StateData#state.usap,
					{'DL', 'ESTABLISH', confirm, undefined}),
			% Discard I queue
			StateData#state{i_queue = []}
	end,
	% Stop T200
	cancel_timer(StateData#state.t200_ref),
	% Start T203
	cancel_timer(StateData#state.t203_ref),
	T203_ref = gen_fsm:send_event_after(StateData#state.t203, t203_expiry),
	% V(S)=0, V(A)=0, V(R)=0
	NewStateData = StateData#state{t200_ref = undefined, t203_ref = T203_ref,
			'V(S)' = 0, 'V(A)' = 0, 'V(R)' = 0},
	{next_state, multiple_frame_established, NewStateData};
awaiting_establishment({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 0:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% F=1? (no)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'D'}),
	{next_state, awaiting_establishment, StateData};
% ref:  ETS 300 125 Figure B-5/Q.921 (2 of 3) 
awaiting_establishment({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 1:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (yes)
	% Discard I queue
	NewStateData = StateData#state{i_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	cancel_timer(NewStateData#state.t200_ref),
	{next_state, tei_assigned, NewStateData#state{t200_ref = undefined}};
awaiting_establishment({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 0:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) when StateData#state.establishable == false ->
	% F=1? (no)
	{next_state, awaiting_establishment, StateData};
awaiting_establishment(t200_expiry, StateData) 
		when StateData#state.rc == StateData#state.n200 ->
	% RC=N200? (yes)
	% Discard I queue
	NewStateData = StateData#state{i_queue = [], t200_ref = undefined},
	% MDL ERROR (G) indication
	gen_fsm:send_event(NewStateData#state.cme, {'MDL', 'ERROR', indication, 'G'}),
	% DL RELEASE indication
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	{next_state, tei_assigned, NewStateData};
awaiting_establishment(t200_expiry, StateData)  ->
	% RC=N200? (no)
	% RC=RC+1
	NewStateData = StateData#state{rc = StateData#state.rc + 1, t200_ref = undefined},
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=1
	SABME = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1, 2#011:3, 1:1, 2#11:2, 2#11:2>>,
	% TX SABME
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, SABME}),
	% Start T200
	T200_ref = gen_fsm:send_event_after(StateData#state.t200, t200_expiry),
	{next_state, awaiting_establishment, NewStateData#state{t200_ref = T200_ref}};
awaiting_establishment({'DL', 'DATA', request, Data}, StateData)  when is_binary(Data),
		StateData#state.layer3_initiated == true ->
	{next_state, awaiting_establishment, StateData};
awaiting_establishment({'DL', 'DATA', request, Data}, StateData)  when is_binary(Data) ->
	% put in I queue
	NewStateData = StateData#state{i_queue = StateData#state.i_queue ++ [Data]},
	{next_state, awaiting_establishment, NewStateData};
%% ref:  ETS 300 125 Figure B-9/Q.921 (1 of 5) 
awaiting_establishment({'DL', 'UNIT DATA', request, Data}, StateData) when is_binary(Data) ->
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=0
	UI = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#000:3, 0:1, 2#00:2, 2#11:2, Data/binary>>,
	% TX UI
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UI}),
	{next_state, awaiting_establishment, StateData};
awaiting_establishment({'PH', 'DATA', indication, 
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, _P:1, 2#00:2, 2#11:2,     % Command (UI)
		Data/binary>>},                    % Information
		StateData) ->
	% DL UNIT DATA indication
	catch gen_fsm:send_event(StateData#state.usap, {'DL', 'UNIT DATA', indication, Data}),
	{next_state, awaiting_establishment, StateData};
% ref:  ETS 300 125 Figure B-9/Q.921 (3 of 5) 
% ref:  ETS 300 125 5.8.5 Frame rejection condition
awaiting_establishment({'PH', 'DATA', indication, _DlParms}, StateData) -> 
	% MDL ERROR indication (L)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'L'}),
	{next_state, awaiting_establishment, StateData};
%awaiting_establishment({'PH', 'DATA', indication, _DlParms}, StateData) -> 
%	% MDL ERROR indication (M,N,O)
%	gen_fsm:send_event(StateData#state.cme, 'M'),
%	gen_fsm:send_event(StateData#state.cme, 'N'),
%	gen_fsm:send_event(StateData#state.cme, 'O'),
%	{next_state, awaiting_establishment, StateData};
awaiting_establishment(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, awaiting_establishment}, Event]),
	{next_state, awaiting_establishment, StateData}.

%% ref:  ETS 300 125 Figure B-6/Q.921 (1 of 2) 
awaiting_release({'MDL', 'REMOVE', request, {_TEI, _CES}}, StateData) ->
	% Discard UI queue
	NewStateData = StateData#state{tei = undefined, ui_queue = []},
	% DL RELEASE confirm
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', confirm, undefined}),
	% Stop T200
	cancel_timer(NewStateData#state.t200_ref),
	{next_state, tei_unassigned, NewStateData#state{tei = undefined, t200_ref = undefined}};
awaiting_release({'PH', 'DEACTIVATION', indication, _PhParms}, StateData) ->
	% Discard UI queue
	NewStateData = StateData#state{ui_queue = []},
	% DL RELEASE confirm
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', confirm, undefined}),
	% Stop T200
	cancel_timer(NewStateData#state.t200_ref),
	{next_state, tei_assigned, NewStateData#state{t200_ref = undefined}};
awaiting_release({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#011:3, P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) ->
	% F=P
	DM = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#000:3, P:1, 2#11:2, 2#11:2>>,
	% TX DM
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, DM}),
	{next_state, awaiting_release, StateData};
awaiting_release({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#010:3, P:1, 2#00:2, 2#11:2>>},   % Command (DISC)
		StateData) ->
	% F=P
	% TX UA
	UA = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#011:3, P:1, 2#00:2, 2#11:2>>,
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UA}),
	{next_state, awaiting_release, StateData};
awaiting_release({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 1:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% F=1? (yes)
	% DL RELEASE confirm
	catch gen_fsm:send_event(StateData#state.usap, {'DL', 'RELEASE', confirm, undefined}),
	% Stop T200
	cancel_timer(StateData#state.t200_ref),
	{next_state, tei_assigned, StateData#state{t200_ref = undefined}};
awaiting_release({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 0:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% F=1? (no)
	% MDL ERROR (D) indication
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'D'}),
	{next_state, awaiting_release, StateData};
% ref:  ETS 300 125 Figure B-6/Q.921 (2 of 2) 
awaiting_release({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 1:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (yes)
	% DL RELEASE confirm
	catch gen_fsm:send_event(StateData#state.usap, {'DL', 'RELEASE', confirm, undefined}),
	% Stop T200
	cancel_timer(StateData#state.t200_ref),
	{next_state, tei_assigned, StateData#state{t200_ref = undefined}};
awaiting_release({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 0:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (no)
	{next_state, awaiting_release, StateData};
awaiting_release(t200_expiry, StateData) 
		when StateData#state.rc == StateData#state.n200 ->
	% RC=N200? (yes)
	% MDL ERROR (H) indication
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'H'}),
	% DL RELEASE confirm
	catch gen_fsm:send_event(StateData#state.usap, {'DL', 'RELEASE', confirm, undefined}),
	{next_state, tei_assigned, StateData#state{t200_ref = undefined}};
awaiting_release(t200_expiry, StateData) ->
	% RC=N200? (no)
	% RC=RC+1
	NewStateData = StateData#state{rc = StateData#state.rc + 1, t200_ref = undefined},
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=1
	DISC = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1, 2#010:3, 1:1, 2#00:2, 2#11:2>>,
	% TX DISC
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, DISC}),
	% Start T200
	T200_ref = gen_fsm:send_event_after(NewStateData#state.t200, t200_expiry),
	{next_state, awaiting_release, NewStateData#state{t200_ref = T200_ref}};
%% ref:  ETS 300 125 Figure B-9/Q.921 (1 of 5) 
awaiting_release({'DL', 'UNIT DATA', request, Data}, StateData) when is_binary(Data) ->
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=0
	UI = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#000:3, 0:1, 2#00:2, 2#11:2, Data/binary>>,
	% TX UI
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UI}),
	{next_state, awaiting_release, StateData};
awaiting_release({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, _P:1, 2#00:2, 2#11:2,     % Command (UI)
		Data/binary>>},                    % Information
		StateData) ->
	% DL UNIT DATA indication
	gen_fsm:send_event(StateData#state.mux, {'DL', 'UNIT DATA', indication, Data}),
	{next_state, awaiting_release, StateData};
% ref:  ETS 300 125 Figure B-9/Q.921 (3 of 5) 
% ref:  ETS 300 125 5.8.5 Frame rejection condition
awaiting_release({'PH', 'DATA', indication, _DlParms}, StateData) -> 
	% MDL ERROR indication (L)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'L'}),
	{next_state, awaiting_release, StateData};
%awaiting_release({'PH', 'DATA', indication, _DlParms}, StateData) -> 
%	% MDL ERROR indication (M,N,O)
%	gen_fsm:send_event(StateData#state.cme, 'M'),
%	gen_fsm:send_event(StateData#state.cme, 'N'),
%	gen_fsm:send_event(StateData#state.cme, 'O'),
%	{next_state, awaiting_release, StateData};
awaiting_release(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, awaiting_release}, Event]),
	{next_state, awaiting_release, StateData}.

%% ref:  ETS 300 125 Figure B-7/Q.921 (1 of 10) 
multiple_frame_established({'DL', 'ESTABLISH', request, _DlParms}, StateData) ->
	% Discard I queue
	% Stop T203
	cancel_timer(StateData#state.t203_ref),
	% Establish data link
	NewStateData = establish_data_link(StateData#state{i_queue = []}),
	% Set layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = true}};
multiple_frame_established({'DL', 'RELEASE', request, _DlParms}, StateData) ->
	% Discard I queue
	% RC=0
	NewStateData = StateData#state{i_queue = [], rc = 0},
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=1
	DISC = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1, 2#010:3, 1:1, 2#00:2, 2#11:2>>,
	% TX DISC
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, DISC}),
	% Stop T203
	% Restart T200
	cancel_timer(NewStateData#state.t203_ref),
	cancel_timer(NewStateData#state.t200_ref),
	T200_ref = gen_fsm:send_event_after(NewStateData#state.t200, t200_expiry),
	{next_state, awaiting_release, NewStateData#state{t200_ref = T200_ref, t203_ref = undefined}};
multiple_frame_established({'DL', 'DATA', request, Data}, StateData) when is_binary(Data),
		StateData#state.peer_receiver_busy == true  ->
	% Peer receiver busy? (yes)
	NewStateData = StateData#state{i_queue = StateData#state.i_queue ++ [Data]},
	{next_state, multiple_frame_esatblished, NewStateData};
multiple_frame_established({'DL', 'DATA', request, Data}, StateData) when is_binary(Data) ->
	% Peer receiver busy? (no)
	NewStateData = transmit_iqueue(StateData#state{i_queue = StateData#state.i_queue ++ [Data], acknowledge_pending = false}),
	% T200 Running?
	case NewStateData#state.t200_ref of
		T200_ref when is_reference(T200_ref) ->
			{next_state, multiple_frame_esatblished, NewStateData};
		_ ->
			% Stop T203
			% Start T200
			cancel_timer(NewStateData#state.t203_ref),
					  T200_ref = gen_fsm:send_event_after(NewStateData#state.t200, t200_expiry),
			{next_state, multiple_frame_esatblished, 
					NewStateData#state{t203_ref = undefined, t200 = T200_ref}}
	end;
% ref:  ETS 300 125 Figure B-7/Q.921 (2 of 10) 
multiple_frame_established(t200_expiry, StateData) ->
	% RC=0
	% Implementation choice available
	% We'll take the easy road for now
	% transmit ENQUIRY
	NewStateData = transmit_enquiry(StateData#state{rc = 0, t200_ref = undefined}),
	% RC=RC+1
	{next_state, timer_recovery, NewStateData#state{rc = StateData#state.rc + 1}};
multiple_frame_established(t203_expiry, StateData) ->
	% transmit ENQUIRY
	NewStateData = transmit_enquiry(StateData#state{t203_ref = undefined}),
	% RC=0
	{next_state, timer_recovery, NewStateData#state{rc = 0}};
multiple_frame_established({'MDL', 'REMOVE', request, {_TEI, _CES}}, StateData) ->
	% Discard I and UI queues
	NewStateData = StateData#state{tei = undefined, i_queue = [], ui_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	% Stop T203
	cancel_timer(NewStateData#state.t200_ref),
	cancel_timer(NewStateData#state.t203_ref),
	{next_state, tei_unassigned, NewStateData#state{t200_ref = undefined, t203_ref = undefined}};
multiple_frame_established({'PH', 'DEACTIVATION', indication, _PhParms}, StateData) ->
	% Discard I and UI queues
	NewStateData = StateData#state{i_queue = [], ui_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	% Stop T203
	cancel_timer(NewStateData#state.t200_ref),
	cancel_timer(NewStateData#state.t203_ref),
	{next_state, tei_unassigned, NewStateData#state{t200_ref = undefined, t203_ref = undefined}};
% ref:  ETS 300 125 Figure B-7/Q.921 (3 of 10) 
multiple_frame_established({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#011:3, P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) ->
	% F=P
	UA = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#011:3, P:1, 2#00:2, 2#11:2>>,
	% TX UA
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UA}),
	% Clear exception consitions
	NextStateData = clear_exception_conditions(StateData),
	% MDL ERROR indication (F)
	gen_fsm:send_event(NextStateData#state.cme, {'MDL', 'ERROR', indication, 'F'}),
	% V(S)=V(A)?
	VA = NextStateData#state.'V(A)',
	case NextStateData#state.'V(S)' of
	VA ->
		NewStateData = NextStateData;
	_ ->
		% Discard I queues
		NewStateData = NextStateData#state{i_queue = []},
		% DL ESTABLISH indication
		catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'ESTABLISH', indication, undefined})
	end,
	% Stop T200
	% Start T203
	cancel_timer(NewStateData#state.t200_ref),
	cancel_timer(NewStateData#state.t203_ref),
	T203_ref = gen_fsm:send_event_after(NewStateData#state.t203, t203_expiry),
	% V(S)=0, V(A)=0, V(R)=0
	{next_state, multiple_frame_established,
			NewStateData#state{t200_ref = undefined, t203_ref = T203_ref,
			'V(S)' = 0, 'V(A)' = 0, 'V(R)' = 0}};
multiple_frame_established({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#010:3, P:1, 2#00:2, 2#11:2>>},   % Command (DISC)
		StateData) ->
	% Discard I queues
	NewStateData = StateData#state{i_queue = []},
	% F=P
	UA = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#011:3, P:1, 2#00:2, 2#11:2>>,
	% TX UA
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, UA}),
	% DL RELEASE indication
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	% Stop T203
	cancel_timer(NewStateData#state.t200_ref),
	cancel_timer(NewStateData#state.t203_ref),
	{next_state, tei_assigned, NewStateData#state{t200_ref = undefined, t203_ref = undefined}};
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 1:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% MDL ERROR indication (C)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'C'}),
	{next_state, multiple_frame_established, StateData};
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 0:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% MDL ERROR indication (D)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'D'}),
	{next_state, multiple_frame_established, StateData};
% ref:  ETS 300 125 Figure B-7/Q.921 (4 of 10) 
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 1:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (yes)
	% MDL ERROR indication (B)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'B'}),
	{next_state, multiple_frame_established, StateData};
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 0:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (no)
	% MDL ERROR indication (E)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'E'}),
	% Stop T203
	cancel_timer(StateData#state.t203_ref),
	% Establish data link
	NewStateData = establish_data_link(StateData),
	% Clear layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = true, t203_ref = undefined}};
multiple_frame_established(set_own_receiver_busy, StateData) when StateData#state.own_receiver_busy == true ->
	{next_state, multiple_frame_established, StateData};
multiple_frame_established(set_own_receiver_busy, StateData) ->
	% Set own receiver busy
	% Clear acknowledge pending
	NewStateData = StateData#state{own_receiver_busy = true, acknowledge_pending = false},
	case StateData#state.role of
		network -> CR = 0;
		user -> CR = 1
	end,
	% F=0
	RNR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#0000010:7, 1:1, (StateData#state.'V(R)'):7, 0:1>>,
	% TX RNR response
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, RNR}),
	{next_state, multiple_frame_established, NewStateData};
multiple_frame_established(clear_own_receiver_busy, StateData) when StateData#state.own_receiver_busy == true ->
	% Clear own receiver busy
	% Clear acknowledge pending
	NewStateData = StateData#state{own_receiver_busy = false, acknowledge_pending = false},
	case StateData#state.role of
		network -> CR = 0;
		user -> CR = 1
	end,
	% F=0
	RR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#0000000:7, 1:1, (StateData#state.'V(R)'):7, 0:1>>,
	% TX RR response
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, RR}),
	{next_state, multiple_frame_established, NewStateData};
multiple_frame_established(clear_own_receiver_busy, StateData) ->
	{next_state, multiple_frame_established, StateData};
% ref:  ETS 300 125 Figure B-7/Q.921 (5 of 10) 
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#0000000:7, 1:1, NR:7, PF:1>>},   % Command (RR)
		StateData) ->
	% Clear peer receiver busy
	NextStateData = transmit_iqueue(StateData#state{peer_receiver_busy = false}),
	% COMMAND?
	NextStateData2 = case CR of
		CR when NextStateData#state.role == network , CR == 0; 
				NextStateData#state.role == user, CR == 1 ->     % command
			% P=1?
			case PF of
				1 ->
					% ENQUIRY response
					enquiry_response(NextStateData);
				0 ->
					NextStateData
			end;
		CR when NextStateData#state.role == network , CR == 1; 
				NextStateData#state.role == user, CR == 0 ->     % response
			% F=1?
			case PF of
				1 ->
					% MDL ERROR indication (A)
					gen_fsm:send_event(NextStateData#state.cme, {'MDL', 'ERROR', indication, 'A'}),
					NextStateData;
				0 ->
					NextStateData
			end
	end,
	% ref:  ETS 300 125 Figure B-7/Q.921 (6 of 10) connector (1)
	% V(A) <= N(R) <= V(S)?
	case validate_nr(NextStateData2#state.'V(A)', NR, NextStateData2#state.'V(S)') of
		true ->
			% V(A)=N(R)
			NewStateData = acknowledge_iqueue(NextStateData2, NR),
			% N(R)=V(S)?
			VS = NewStateData#state.'V(S)',
			case NR of
				VS ->                                % true
					% Stop T200
					% Start T203
					cancel_timer(NewStateData#state.t200_ref),
					cancel_timer(NewStateData#state.t203_ref),
					T203_ref = gen_fsm:send_event_after(NewStateData#state.t203, t203_expiry),
					{next_state, multiple_frame_established,
							NewStateData#state{t200_ref = undefined, t203_ref = T203_ref}};
				_ ->                                 % false
					% Restart T200
					cancel_timer(NewStateData#state.t200_ref),
					T200_ref = gen_fsm:send_event_after(NewStateData#state.t200, t200_expiry),
					{next_state, multiple_frame_established, NewStateData#state{t200_ref = T200_ref}}
			end;
		false ->
			% N(R) error recovery
			% Stop T203
			cancel_timer(NextStateData#state.t203_ref),
			{next_state, awaiting_establishment, nr_error_recovery(NextStateData2#state{t203_ref = undefined})}
	end;
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#0000100:7, 1:1, NR:7, PF:1>>},     % Command (REJ)
		StateData) ->
	% Clear peer receiver busy
	NextStateData = transmit_iqueue(StateData#state{peer_receiver_busy = false}),
	% COMMAND?
	NextStateData2 = case CR of
		CR when NextStateData#state.role == network , CR == 0; 
				NextStateData#state.role == user, CR == 1 ->     % command
			% P=1?
			case PF of
				1 ->
					% ENQUIRY response
					enquiry_response(NextStateData);
				_ ->
					NextStateData
			end;
		CR when NextStateData#state.role == network , CR == 1; 
				NextStateData#state.role == user, CR == 0 ->     % response
			% F=1?
			case PF of
				1 ->
					% MDL ERROR indication (A)
					gen_fsm:send_event(NextStateData#state.cme, {'MDL', 'ERROR', indication, 'A'}),
					NextStateData;
				_ ->
					NextStateData
			end
	end,
	% ref:  ETS 300 125 Figure B-7/Q.921 (6 of 10) connector (2)
	% V(A) <= N(R) <= V(S)?
	case validate_nr(NextStateData2#state.'V(A)', NR, NextStateData2#state.'V(S)') of
		true ->
			% V(A)=N(R)
			NextStateData3 = acknowledge_iqueue(NextStateData2, NR),
			NewStateData = transmit_iqueue(NextStateData3#state{'V(S)' = NR}),
			% Stop T200
			% Start T203
			cancel_timer(NewStateData#state.t200_ref),
			cancel_timer(NewStateData#state.t203_ref),
			T203_ref = gen_fsm:send_event_after(NewStateData#state.t203, t203_expiry),
			{next_state, multiple_frame_established,
					NewStateData#state{t200_ref = undefined, t203_ref = T203_ref}};
		false ->
			% N(R) error recovery
			% Stop T203
			cancel_timer(NextStateData2#state.t203_ref),
			{next_state, awaiting_establishment, nr_error_recovery(NextStateData2#state{t203_ref = undefined})}
	end;
% ref:  ETS 300 125 Figure B-7/Q.921 (7 of 10) 
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#0000010:7, 1:1, NR:7, PF:1>>},     % Command (RNR)
		StateData) ->
	% Set peer receiver busy
	NextStateData = transmit_iqueue(StateData#state{peer_receiver_busy = false}),
	% COMMAND?
	NextStateData2 = case CR of
		CR when NextStateData#state.role == network , CR == 0; 
				NextStateData#state.role == user, CR == 1 ->     % command
			% P=1?
			case PF of
				1 ->
					% ENQUIRY response
					enquiry_response(NextStateData);
				_ ->
					NextStateData
			end;
		CR when NextStateData#state.role == network , CR == 1; 
				NextStateData#state.role == user, CR == 0 ->     % response
			% F=1?
			case PF of
				1 ->
					% MDL ERROR indication (A)
					gen_fsm:send_event(NextStateData#state.cme, {'MDL', 'ERROR', indication, 'A'}),
					NextStateData;
				_ ->
					NextStateData
			end
	end,
	% V(A) <= N(R) <= V(S)?
	case validate_nr(NextStateData2#state.'V(A)', NR, NextStateData2#state.'V(S)') of
		true ->
			% V(A)=N(R)
			NewStateData = acknowledge_iqueue(NextStateData2, NR),
			% Stop T200
			% Start T203
			cancel_timer(NewStateData#state.t200_ref),
			cancel_timer(NewStateData#state.t203_ref),
			T203_ref = gen_fsm:send_event_after(NewStateData#state.t203, t203_expiry),
			{next_state, multiple_frame_established,
					NewStateData#state{t200_ref = undefined, t203_ref = T203_ref}};
		false ->
			% N(R) error recovery
			% Stop T203
			cancel_timer(NextStateData2#state.t203_ref),
			{next_state, awaiting_establishment, nr_error_recovery(NextStateData2#state{t203_ref = undefined})}
	end;
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#100:3, _F:1, 2#01:2, 2#11:2>>},   % Command (FRMR)
		StateData) ->
	% MDL ERROR indication (K)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'K'}),
	% establish data link
	NewStateData = establish_data_link(StateData),
	% Stop T203
	cancel_timer(NewStateData#state.t203_ref),
	% clear layer3_initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = false, t203_ref = undefined}};
% ref:  ETS 300 125 Figure B-7/Q.921 (8 of 10) 
multiple_frame_established({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		NS:7, 0:1, NR:7, P:1,              % Command (I)
		Data/binary>>}, StateData) ->
	VA = StateData#state.'V(A)',
	VS = StateData#state.'V(S)',
	VR = StateData#state.'V(R)',
	StateData2 = case StateData#state.own_receiver_busy of
		true ->
			% Discard INFORMATION
			% P=1?
			case P of
				1 ->
					% F=1
					RNR = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#0000010:7, 1:1, (StateData#state.'V(R)'):7, 1:1>>,
					% TX RNR
					gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RNR}),
					% Clear acknowledge pending
					StateData#state{acknowledge_pending = false};
				0 ->
					StateData
			end;
		_ ->
			% N(S)=V(R)?
			case NS of
				VR ->
					%V(R)=V(R)+1
					NewVR = StateData#state.'V(R)' + 1,
					% Clear reject exception (deferred to below)
					% DL DATA indication
					catch gen_fsm:send_event(StateData#state.usap, {'DL', 'DATA', indication, Data}),
					% P=1?
					case P of
						1 ->
							% F=P
							RNR = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#0000010:7, 1:1, NewVR:7, P:1>>,
							% TX RNR
							gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RNR}),
							% Clear acknowledge pending
							StateData#state{'V(R)' = NewVR, acknowledge_pending = false,
									% Clear reject exception (from above)
									reject_exception = false};
						0 ->
							% acknowledge pending?
							case StateData#state.acknowledge_pending of
								true ->
									StateData;
								_ ->
									% ACKNOWLEDGE PENDING
									gen_fsm:send_event(self(), 'ACKNOWLEDGE PENDING'),
									% Set acknowledge pending
									StateData#state{'V(R)' = NewVR, acknowledge_pending = true}
							end
					end;
				_ ->
					% Discard INFORMATION
					% Reject Exception?
					case StateData#state.reject_exception of
						true ->
							% P=1?
							case P of
								1 ->
									% F=P
									RR = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#0000000:7, 1:1, (StateData#state.'V(R)'):7, P:1>>,
									% TX RR
									gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RR}),
									% Clear acknowledge pending
									StateData#state{acknowledge_pending = false};
								0 ->
									StateData
							end;
						_ ->
							% F=P
							REJ = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#0000100:7, 1:1, (StateData#state.'V(R)'):7, P:1>>,
							% TX REJ
							gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, REJ}),
							% Set reject exception
							% Clear acknowledge pending
							StateData#state{reject_exception = false, acknowledge_pending = false}
					end
			end
	end,
	% ref:  ETS 300 125 Figure B-7/Q.921 (9 of 10) connector (3)
	% V(A) <= N(R) <= V(S)?
	case validate_nr(StateData2#state.'V(A)', NR, StateData2#state.'V(S)') of
		true ->
			% V(A)=N(R)
			NextStateData = acknowledge_iqueue(StateData2, NR),
			% Peer receiver busy?
			case NextStateData#state.peer_receiver_busy of
				true ->
					% V(A)=N(R)
					{next_state, multiple_frame_established, NextStateData};
				_ ->
					% N(R)=V(S)?
					VS = NextStateData#state.'V(S)',
					case NR of
						VS ->          % true
							% Stop T200
							% Restart T203
							cancel_timer(NextStateData#state.t200_ref),
							cancel_timer(NextStateData#state.t203_ref),
							T203_ref = gen_fsm:send_event_after(NextStateData#state.t203, t203_expiry),
							NewStateData = NextStateData#state{'V(A)' = NR,
									t200_ref = undefined, t203_ref = T203_ref },
							{next_state, multiple_frame_established, NewStateData};
						_ ->                           % false
							% N(R)=V(A)?
							case NR of
								VA ->    % true
									{next_state, multiple_frame_established, NextStateData};
								_ ->                     % false
									% Restart T200
									cancel_timer(NextStateData#state.t200_ref),
									T200_ref = gen_fsm:send_event_after(NextStateData#state.t200, t200_expiry),
									NewStateData = NextStateData#state{'V(A)' = NR, t200_ref = T200_ref},
									{next_state, multiple_frame_established, NewStateData}
							end
					end
			end;
		false ->
			% N(R) error recovery
			% Stop T203
			cancel_timer(StateData#state.t203_ref),
			{next_state, awaiting_establishment, nr_error_recovery(StateData#state{t203_ref = undefined})}
	end;
% ref:  ETS 300 125 Figure B-7/Q.921 (10 of 10) 
multiple_frame_established('ACKNOWLEDGE PENDING', StateData) 
		when StateData#state.acknowledge_pending == true ->
	% Acknowledge pending? (yes)
	case StateData#state.role of
		network -> CR = 0;
		user -> CR = 1
	end,
	% F=0
	RR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#0000000:7, 1:1, (StateData#state.'V(R)'):7, 0:1>>,
	% TX RR
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RR}),
	% Clear acknowledge pending
	NewStateData = StateData#state{acknowledge_pending = false},
	{next_state, multiple_frame_established, NewStateData};
multiple_frame_established('ACKNOWLEDGE PENDING', StateData) ->
	% Acknowledge pending? (no)
	{next_state, multiple_frame_established, StateData};
%% ref:  ETS 300 125 Figure B-9/Q.921 (1 of 5) 
multiple_frame_established({'DL', 'UNIT DATA', request, Data}, StateData) when is_binary(Data) ->
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=0
	UI = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#000:3, 0:1, 2#00:2, 2#11:2, Data/binary>>,
	% TX UI
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UI}),
	{next_state, multiple_frame_established, StateData};
multiple_frame_established({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, _P:1, 2#00:2, 2#11:2,     % Command (UI)
		Data/binary>>},                    % Information
		StateData) ->
	% DL UNIT DATA indication
	gen_fsm:send_event(StateData#state.mux, {'DL', 'UNIT DATA', indication, Data}),
	{next_state, multiple_frame_established, StateData};
% ref:  ETS 300 125 Figure B-9/Q.921 (2 of 5) 
% ref:  ETS 300 125 5.8.5 Frame rejection condition
multiple_frame_established({'PH', 'DATA', indication, _DlParms}, StateData) -> 
	% MDL ERROR indication (L)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'L'}),
	{next_state, multiple_frame_established, StateData};
%multiple_frame_established({'PH', 'DATA', indication, _DlParms}, StateData) -> 
%	% MDL ERROR indication (M,N,O)
%	gen_fsm:send_event(StateData#state.cme, 'M'),
%	gen_fsm:send_event(StateData#state.cme, 'N'),
%	gen_fsm:send_event(StateData#state.cme, 'O'),
%	{next_state, multiple_frame_established, StateData};
multiple_frame_established(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, multiple_frame_established}, Event]),
	{next_state, multiple_frame_established, StateData}.


% ref:  ETS 300 125 Figure B-8/Q.921 (1 of 9) 
timer_recovery({'DL', 'ESTABLISH', request, _DlParms}, StateData) ->
	% Discard I queue
	% Establish data link
	NewStateData = establish_data_link(StateData#state{i_queue = []}),
	% Set layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = true}};
timer_recovery({'DL', 'RELEASE', request, _DlParms}, StateData) ->
	% Discard I queue
	% RC=0
	NewStateData = StateData#state{i_queue = [], rc = 0},
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=1
	DISC = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1, 2#010:3, 1:1, 2#00:2, 2#11:2>>,
	% TX DISC
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, DISC}),
	{next_state, awaiting_release, NewStateData};
timer_recovery({'DL', 'DATA', request, Data}, StateData) when is_binary(Data) ->
	% Put in I queue
	NewStateData = StateData#state{i_queue = StateData#state.i_queue ++ [Data]},
	{next_state, timer_recovery, NewStateData};
% ref:  ETS 300 125 Figure B-8/Q.921 (2 of 9) 
timer_recovery(t200_expiry, StateData) 
		when StateData#state.rc == StateData#state.n200 ->
	% RC=N200? (yes)
	% MDL ERROR indication
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'I'}),
	% Establish data link
	NewStateData = establish_data_link(StateData#state{t200_ref = undefined}),
	% Clear layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = false}};
timer_recovery(t200_expiry, StateData) ->
	% RC=N200? (no)
	% V(S)=V(A)?
	VA = StateData#state.'V(A)',
	case StateData#state.'V(S)' of
		VA ->                 % true
			% Transmit enquiry
			NextStateData = transmit_enquiry(StateData#state{t200_ref = undefined}),
			% RC=RC+1
			NewStateData= NextStateData#state{rc = NextStateData#state.rc + 1},
			{next_state, timer_recovery, NewStateData};
		_ ->                                  % false
			% Implementation choice available
			% We'll take the easy road for now
			% Transmit enquiry
			NextStateData = transmit_enquiry(StateData#state{t200_ref = undefined}),
			% RC=RC+1
			NewStateData= NextStateData#state{rc = NextStateData#state.rc + 1},
			{next_state, timer_recovery, NewStateData}
	end;
timer_recovery({'MDL', 'REMOVE', request, {_TEI, _CES}}, StateData) ->
	% Discard I and UI queues
	NewStateData = StateData#state{tei = undefined, i_queue = [], ui_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	cancel_timer(NewStateData#state.t200_ref),
	{next_state, tei_unassigned, NewStateData#state{t200_ref = undefined}};
timer_recovery({'PH', 'DEACTIVATE', indication, _PhParms}, StateData) ->
	% Discard I and UI queues
	NewStateData = StateData#state{i_queue = [], ui_queue = []},
	% DL RELEASE indication
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	cancel_timer(NewStateData#state.t200_ref),
	{next_state, tei_assigned, NewStateData#state{t200_ref = undefined}};
% ref:  ETS 300 125 Figure B-8/Q.921 (3 of 9) 
timer_recovery({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#011:3, P:1, 2#11:2, 2#11:2>>},   % Command (SABME)
		StateData) ->
	% F=P
	UA = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#011:3, P:1, 2#00:2, 2#11:2>>,
	% TX UA
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UA}),
	% Clear exception conditions
	NextStateData = clear_exception_conditions(StateData),
	% MDL ERROR indication (F)
	gen_fsm:send_event(NextStateData#state.cme, {'MDL', 'ERROR', indication, 'F'}),
	% V(S)=V(A)?
	VA = NextStateData#state.'V(A)',
	NewStateData = case NextStateData#state.'V(S)' of
		VA ->                    % true
			NextStateData;
		_ ->                                         % false
			% DL ESTABLISH indication
			catch gen_fsm:send_event(NextStateData#state.usap, {'DL', 'ESTABLISH', indication, undefined}),
			% Discard I queue
			NextStateData#state{i_queue = []}
	end,
	% Stop T200
	% Start T203
	cancel_timer(NewStateData#state.t200_ref),
	cancel_timer(NewStateData#state.t203_ref),
	T203_ref = gen_fsm:send_event_after(NewStateData#state.t203, t203_expiry),
	% V(S)=0, V(A)=0, V(R)=0
	{next_state, multiple_frame_established, NewStateData#state{t200_ref = undefined,
			t203_ref = T203_ref, 'V(S)' = 0, 'V(A)' = 0, 'V(R)' = 0}};
timer_recovery({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		2#010:3, P:1, 2#00:2, 2#11:2>>},   % Command (DISC)
		StateData) ->
	% Discard I queue
	NewStateData = StateData#state{i_queue = []},
	% F=P
	UA = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#011:3, P:1, 2#00:2, 2#11:2>>,
	% TX UA
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, UA}),
	% DL RELEASE indication
	catch gen_fsm:send_event(NewStateData#state.usap, {'DL', 'RELEASE', indication, undefined}),
	% Stop T200
	cancel_timer(NewStateData#state.t200_ref),
	{next_state, tei_assigned, NewStateData#state{t200_ref = undefined}};
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 1:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% F=1? (yes)
	% MDL ERROR indication (C)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'C'}),
	{next_state, timer_recovery, StateData};
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#011:3, 0:1, 2#00:2, 2#11:2>>},   % Command (UA)
		StateData) ->
	% F=1? (no)
	% MDL ERROR indication (D)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'D'}),
	{next_state, timer_recovery, StateData};
% ref:  ETS 300 125 Figure B-8/Q.921 (4 of 9) 
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 1:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (yes)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'B'}),
	% Establish data link
	NewStateData = establish_data_link(StateData),
	% Clear layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = false}};
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, 0:1, 2#11:2, 2#11:2>>},   % Command (DM)
		StateData) ->
	% F=1? (no)
	% MDL ERROR indication (E)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'E'}),
	% Establish data link
	NewStateData = establish_data_link(StateData),
	% Clear layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = false}};
timer_recovery('SET OWN RECEIVER BUSY', StateData) 
		when StateData#state.own_receiver_busy == true ->
	{next_state, timer_recovery, StateData};
timer_recovery('SET OWN RECEIVER BUSY', StateData) ->
	case StateData#state.role of
		network -> CR = 0;
		user -> CR = 1
	end,
	% F=0
	RNR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#0000010:7, 1:1, (StateData#state.'V(R)'):7, 0:1>>,
	% TX RNR response
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RNR}),
	% Set own receiver busy
	% Clear acknowledge pending
	NewStateData = StateData#state{own_receiver_busy = true, acknowledge_pending = false},
	{next_state, timer_recovery, NewStateData};
timer_recovery('CLEAR OWN RECEIVER BUSY', StateData) 
		when StateData#state.own_receiver_busy == false ->
	{next_state, timer_recovery, StateData};
timer_recovery('CLEAR OWN RECEIVER BUSY', StateData)  ->
	case StateData#state.role of
		network -> CR = 0;
		user -> CR = 1
	end,
	% F=0
	RR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#0000000:7, 1:1, (StateData#state.'V(R)'):7, 0:1>>,
	% TX RR response
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RR}),
	% Clear own receiver busy
	% Clear acknowledge pending
	NewStateData = StateData#state{own_receiver_busy = false, acknowledge_pending = false},
	{next_state, timer_recovery, NewStateData};
% ref:  ETS 300 125 Figure B-8/Q.921 (5 of 9) 
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		Command:7, 1:1, NR:7, P:1>>},        % Command
		StateData) when
		StateData#state.role == network, CR == 0, Command == 2#0000000;   % Network side Command (RR) 
		StateData#state.role == user, CR == 1, Command == 2#0000000;      % User side Command (RR)
		StateData#state.role == network, CR == 0, Command == 2#0000100;   % Network side Command (REJ) 
		StateData#state.role == user, CR == 1, Command == 2#0000100 ->    % User side Command (REJ)
	% Clear peer receiver busy
	NextStateData = StateData#state{peer_receiver_busy = false},
	% P=1?
	NewStateData = case P of
		1 ->
			% Enquiry response
			enquiry_response(NextStateData);
		0 ->
			NextStateData
	end,
	% V(A)<=N(R)<=V(S)?
	case validate_nr(NewStateData#state.'V(A)', NR, NewStateData#state.'V(S)') of
		true ->
			% V(A)=N(R)
			{next_state, timer_recovery, acknowledge_iqueue(NewStateData, NR)};
		false ->
			% N(R) Error recovery
			{next_state, awaiting_establishment, nr_error_recovery(NewStateData)}
	end;
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		Response:7, 1:1, NR:7, F:1>>},       % Response
		StateData) when
		StateData#state.role == network, CR == 1, Response == 2#0000000;   % Network side Response (RR) 
		StateData#state.role == user, CR == 0, Response == 2#0000000;      % User side Response (RR)
		StateData#state.role == network, CR == 1, Response == 2#0000010;   % Network side Response (REJ) 
		StateData#state.role == user, CR == 0, Response == 2#0000010 ->    % User side Response (REJ)
	% Clear peer receiver busy
	NextStateData = StateData#state{peer_receiver_busy = false},
	% V(A)<=N(R)<=V(S)?
	case validate_nr(NextStateData#state.'V(A)', NR, NextStateData#state.'V(S)') of
		true ->
			% V(A)=N(R)
			NextStateData2 = acknowledge_iqueue(NextStateData, NR),
			% F=1?
			case F of
				1 ->
					% Stop T200
					% Start T203
					cancel_timer(NextStateData2#state.t200_ref),
					cancel_timer(NextStateData2#state.t203_ref),
					T203_ref = gen_fsm:send_event_after(NextStateData2#state.t203, t203_expiry),
					NewStateData = NextStateData2#state{t200_ref = undefined, t203_ref = T203_ref},
					% Invoke retransmission
					{next_state, multiple_frame_established, transmit_iqueue(NewStateData#state{'V(S)' = NR})};
				0 ->
					{next_state, timer_recovery, NextStateData2}
			end;
		false ->
			% N(R) Error recovery
			{next_state, awaiting_establishment, nr_error_recovery(NextStateData)}
	end;
% ref:  ETS 300 125 Figure B-8/Q.921 (6 of 9) 
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		Command:7, 1:1, NR:7, P:1>>},        % Command
		StateData) when
		StateData#state.role == network, CR == 0, Command == 2#0000010;   % Network side Command (RNR) 
		StateData#state.role == user, CR == 1, Command == 2#0000010 ->    % User side Command (RNR)
	% Set peer receiver busy
	NextStateData = StateData#state{peer_receiver_busy = true},
	% P=1?
	NewStateData = case P of
		1 ->
			% Enquiry response
			enquiry_response(NextStateData);
		0 ->
			NextStateData
	end,
	% V(A)<=N(R)<=V(S)?
	case validate_nr(NewStateData#state.'V(A)', NR, NewStateData#state.'V(S)') of
		true ->
			% V(A)=N(R)
			{next_state, timer_recovery, acknowledge_iqueue(NewStateData, NR)};
		false ->
			% N(R) Error recovery
			{next_state, awaiting_establishment, nr_error_recovery(NewStateData)}
	end;
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, CR:1, 0:1, _TEI:7, 1:1,   % Address
		Response:7, 1:1, NR:7, F:1>>},       % Response
		StateData) when
		StateData#state.role == network, CR == 0, Response == 2#0000010;   % Network side Response (RNR) 
		StateData#state.role == user, CR == 1, Response == 2#0000010 ->    % User side Response (RNR)
	% Set peer receiver busy
	NextStateData = StateData#state{peer_receiver_busy = true},
	% V(A)<=N(R)<=V(S)?
	case validate_nr(NextStateData#state.'V(A)', NR, NextStateData#state.'V(S)') of
		true ->
			% V(A)=N(R)
			NextStateData2 = acknowledge_iqueue(NextStateData, NR),
			% F=1?
			case F of
				1 ->
					% Restart T200
					cancel_timer(NextStateData2#state.t200_ref),
					T200_ref = gen_fsm:send_event_after(NextStateData2#state.t200, t200_expiry),
					NewStateData = NextStateData2#state{t200_ref = T200_ref},
					% Invoke retransmission
					{next_state, multiple_frame_established, transmit_iqueue(NewStateData#state{'V(S)' = NR})};
				0 ->
					{next_state, timer_recovery, NextStateData2}
			end;
		false ->
			% N(R) Error recovery
			{next_state, awaiting_establishment, nr_error_recovery(NextStateData)}
	end;
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#100:3, _F:1, 2#01:2, 2#11:2>>},     % Command (FRMR)
		StateData) ->
	% MDL ERROR indication (K)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'K'}),
	% Establish data link
	NewStateData = establish_data_link(StateData),
	% Clear layer 3 initiated
	{next_state, awaiting_establishment, NewStateData#state{layer3_initiated = false}};
% ref:  ETS 300 125 Figure B-8/Q.921 (7 of 9)
timer_recovery({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		_NS:7, 0:1, NR:7, P:1,              % Command (I)
		_Data/binary>>}, StateData) when StateData#state.own_receiver_busy == true ->
	% Own receiver busy? (yes)
	% Discard Information
	% P=1?
	NextStateData = case P of
		1 ->
			% F=1
			RNR = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#0000010:7, 1:1, (StateData#state.'V(R)'):7, 1:1>>,
			% TX RNR
			gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RNR}),
			% Clear acknowledge pending		
			StateData#state{acknowledge_pending = false};
		_ ->
			StateData
	end,
	% ref:  ETS 300 125 Figure B-8/Q.921 (8 of 9) connector (4)
	% V(A)<=N(R)<=V(S)?
	case validate_nr(NextStateData#state.'V(A)', NR, NextStateData#state.'V(S)') of
		true ->
			% V(A)=N(R)
			NewStateData = acknowledge_iqueue(NextStateData, NR),
			{next_state, timer_recovery, NewStateData};
		false ->
			% N(R) Error recovery
			{next_state, awaiting_establishment, nr_error_recovery(NextStateData)}
	end;
timer_recovery({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		NS:7, 0:1, NR:7, P:1,              % Command (I)
		Data/binary>>}, StateData) when NS == StateData#state.'V(R)' ->
	% Own receiver busy? (no)
	% N(S)=V(R)? (yes)
	% V(R)=V(R)+1
	% Clear reject_exception
	NextStateData = StateData#state{'V(R)' = (StateData#state.'V(R)' + 1) rem 128,
			reject_exception = false},
	% DL DATA indication
	catch gen_fsm:send_event(NextStateData#state.usap, {'DL', 'DATA', indication, Data}),
	% P=1?
	NewStateData = case P of
		1 ->
			% F=P
			RR = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#0000000:7, 1:1, (StateData#state.'V(R)'):7, P:1>>,
			% TX RR
			gen_fsm:send_event(NextStateData#state.mux, {'PH', 'DATA', request, RR}),
			% Clear acknowledge pending
			NextStateData#state{acknowledge_pending = false};
		0 ->
			% Acknowledge pending?
			case NextStateData#state.acknowledge_pending of
				true ->
					NextStateData;
				_ ->
					% Acknowledge pending
					gen_fsm:send_event(self(), 'ACKNOWLEDGE PENDING'),
					NextStateData#state{acknowledge_pending = true}
			end
	end,
	% ref:  ETS 300 125 Figure B-8/Q.921 (8 of 9) connector (4)
	% V(A)<=N(R)<=V(S)?
	case validate_nr(NewStateData#state.'V(A)', NR, NewStateData#state.'V(S)') of
		true ->
			% V(A)=N(R)
			{next_state, timer_recovery, acknowledge_iqueue(NextStateData, NR)};
		false ->
			% N(R) Error recovery
			{next_state, awaiting_establishment, nr_error_recovery(NewStateData)}
	end;
timer_recovery({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		_NS:7, 0:1, NR:7, P:1,              % Command (I)
		_Data/binary>>}, StateData) when StateData#state.reject_exception == true ->
	% Own receiver busy? (no)
	% N(S)=V(R)? (no)
	% Discard information
	% Reject exception? (yes)
	% P=1?
	NewStateData = case P of
		1 ->
			% F=P
			RR = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#0000000:7, 1:1, (StateData#state.'V(R)'):7, P:1>>,
			% TX RR
			gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RR}),
			% Clear acknowledge pending
			StateData#state{acknowledge_pending = false};
		0 ->
			StateData
	end,
	% ref:  ETS 300 125 Figure B-8/Q.921 (8 of 9) connector (4)
	% V(A)<=N(R)<=V(S)?
	case validate_nr(NewStateData#state.'V(A)', NR, NewStateData#state.'V(S)') of
		true ->
			% V(A)=N(R)
			{next_state, timer_recovery, acknowledge_iqueue(NewStateData, NR)};
		false ->
			% N(R) Error recovery
			{next_state, awaiting_establishment, nr_error_recovery(NewStateData)}
	end;
timer_recovery({'PH', 'DATA', indication,
		<<SAPI:6, CR:1, 0:1, TEI:7, 1:1,   % Address
		_NS:7, 0:1, NR:7, P:1,              % Command (I)
		_Data/binary>>}, StateData) ->
	% Own receiver busy? (no)
	% N(S)=V(R)? (no)
	% Discard information
	% Reject exception? (no)
	% F=P
	REJ = <<SAPI:6, CR:1, 0:1, TEI:7, 1:1, 2#0000100:7, 1:1, (StateData#state.'V(R)'):7, P:1>>,
	% TX REJ
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, REJ}),
	% Clear acknowledge pending
	StateData#state{acknowledge_pending = false},
	% ref:  ETS 300 125 Figure B-8/Q.921 (8 of 9) connector (4)
	% V(A)<=N(R)<=V(S)?
	case validate_nr(StateData#state.'V(A)', NR, StateData#state.'V(S)') of
		true ->
			% V(A)=N(R)
			{next_state, timer_recovery, acknowledge_iqueue(StateData, NR)};
		false ->
			% N(R) Error recovery
			{next_state, awaiting_establishment, nr_error_recovery(StateData)}
	end;
% ref:  ETS 300 125 Figure B-8/Q.921 (9 of 9)
timer_recovery('ACKNOWLEDGE PENDING', StateData) 
		when StateData#state.acknowledge_pending == true ->
	% Acknowledge pending? (yes)
	% Clear acknowledge pending
	case StateData#state.role of
		network -> CR = 0;
		user -> CR = 1
	end,
	% F=0
	RR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#0000000:7, 1:1, (StateData#state.'V(R)'):7, 0:1>>,
	NewStateData = StateData#state{acknowledge_pending = false},
	% TX RR
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, RR}),
	{next_state, timer_recovery, NewStateData};
timer_recovery('ACKNOWLEDGE PENDING', StateData) ->
	% Acknowledge pending? (no)
	{next_state, timer_recovery, StateData};
%% ref:  ETS 300 125 Figure B-9/Q.921 (1 of 5) 
timer_recovery({'DL', 'UNIT DATA', request, Data}, StateData) when is_binary(Data) ->
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=0
	UI = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#000:3, 0:1, 2#00:2, 2#11:2, Data/binary>>,
	% TX UI
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UI}),
	{next_state, timer_recovery, StateData};
timer_recovery({'PH', 'DATA', indication,
		<<_SAPI:6, _CR:1, 0:1, _TEI:7, 1:1,   % Address
		2#000:3, _P:1, 2#00:2, 2#11:2,     % Command (UI)
		Data/binary>>},                    % Information
		StateData) ->
	% DL UNIT DATA indication
	gen_fsm:send_event(StateData#state.mux, {'DL', 'UNIT DATA', indication, Data}),
	{next_state, timer_recovery, StateData};
% ref:  ETS 300 125 Figure B-9/Q.921 (2 of 5) 
% ref:  ETS 300 125 5.8.5 Frame rejection condition
timer_recovery({'PH', 'DATA', indication, _DlParms}, StateData) -> 
	% MDL ERROR indication (L)
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'L'}),
	{next_state, timer_recovery, StateData};
%timer_recovery({'PH', 'DATA', indication, _DlParms}, StateData) -> 
%	% MDL ERROR indication (M,N,O)
%	gen_fsm:send_event(StateData#state.cme, 'M'),
%	gen_fsm:send_event(StateData#state.cme, 'N'),
%	gen_fsm:send_event(StateData#state.cme, 'O'),
%	{next_state, timer_recovery, StateData};
timer_recovery(Event, StateData) ->
	error_logger:info_report(["Unhandled message", {module, ?MODULE},
			{state, timer_recovery}, Event]),
	{next_state, timer_recovery, StateData}.

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
	catch gen_fsm:send_all_state_event(StateData#state.mux, {close, {p2p, StateData#state.sapi, self()}}).

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------

%% ref: ETS 300 125 Figure B-9/Q.921 (4 of 5)
nr_error_recovery(StateData) ->
	% MDL ERROR indication
	gen_fsm:send_event(StateData#state.cme, {'MDL', 'ERROR', indication, 'J'}),
	% Clear layer 3 initiated
	establish_data_link(StateData#state{layer3_initiated = false}).

%% ref: ETS 300 125 Figure B-9/Q.921 (4 of 5)
establish_data_link(StateData) ->
	% Clear exception conditions
	NewStateData = clear_exception_conditions(StateData),
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=1
	SABME = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1, 2#011:3, 1:1, 2#11:2, 2#11:2>>,
	% TX SABME
	gen_fsm:send_event(NewStateData#state.mux, {'PH', 'DATA', request, SABME}),
	% RC=0
	% Restart T200
	% Stop T203
	cancel_timer(NewStateData#state.t200_ref),
	cancel_timer(NewStateData#state.t203_ref),
	T203_ref = gen_fsm:send_event_after(NewStateData#state.t203, t203_expiry),
	NewStateData#state{rc = 0, t200_ref = undefined, t203_ref = T203_ref}.

%% ref: ETS 300 125 Figure B-9/Q.921 (4 of 5)
clear_exception_conditions(StateData) ->
	% Clear peer receiver busy
	% Clear reject exception
	% Clear own_receiver_busy
	% Clear acknowledge pending
	StateData#state{peer_receiver_busy = false,
			reject_exception = false,
			own_receiver_busy = false,
			acknowledge_pending = false}.

%% ref: ETS 300 125 Figure B-9/Q.921 (4 of 5)
transmit_enquiry(StateData) ->
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% Own receiver busy?
	case StateData#state.own_receiver_busy of
		true ->
			% P=1
			RNR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7,
					1:1, 2#0000010:7, 1:1, (StateData#state.'V(R)'):7, 1:1>>,
			% TX RNR Command
			gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RNR});
		_ ->
			% P=1
			RR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7,
					1:1, 2#0000000:7, 1:1, (StateData#state.'V(R)'):7, 1:1>>,
			% TX RR Command
			gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RR})
	end,
	% Clear acknowledge pending
	% Start T200
	T200_ref = gen_fsm:send_event_after(StateData#state.t200, t200_expiry),
	StateData#state{acknowledge_pending = false, t200_ref = T200_ref}.

%% ref: ETS 300 125 Figure B-9/Q.921 (5 of 5)
enquiry_response(StateData) ->
	case StateData#state.role of
		network -> CR = 0;
		user -> CR = 1
	end,
	% Own receiver busy?
	case StateData#state.own_receiver_busy of
		true ->
			% F=1
			RNR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7,
					1:1, 2#0000010:7, 1:1, (StateData#state.'V(R)'):7, 1:1>>,
			% TX RNR Response
			gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RNR});
		_ ->
			% F=1
			RR = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7,
					1:1, 2#0000000:7, 1:1, (StateData#state.'V(R)'):7, 1:1>>,
			% TX RR Response
			gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, RR})
	end,
	% Clear acknowledge pending
	StateData#state{acknowledge_pending = false}.

cancel_timer(TimerRef) when is_reference(TimerRef) ->
	gen_fsm:cancel_timer(TimerRef);
cancel_timer(_) -> ok.

acknowledge_iqueue(StateData, NR) ->
	NumberOfAcknowledged = modulo_subtract(NR, StateData#state.'V(A)'), 
	NewQueue = lists:nthtail(NumberOfAcknowledged, StateData#state.i_queue),
	StateData#state{i_queue = NewQueue, 'V(A)' = NR}.

transmit_iqueue(StateData) when StateData#state.peer_receiver_busy == 1;
		StateData#state.'V(S)' == ((StateData#state.'V(A)' + StateData#state.k) rem 128) ->
	StateData;
transmit_iqueue(StateData) ->
	NumberOfUnacknowledged = modulo_subtract(StateData#state.'V(S)', StateData#state.'V(A)'),
	PendingTransmission = lists:nthtail(NumberOfUnacknowledged, StateData#state.i_queue),
	NumberOfSendable = StateData#state.k - NumberOfUnacknowledged,
	send_iframes(StateData, lists:sublist(PendingTransmission, NumberOfSendable)),
	StateData#state{'V(S)' = modulo_add(StateData#state.'V(S)', NumberOfSendable)}.

send_iframes(_, []) -> ok;
send_iframes(StateData, [Data|T]) ->
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	% P=0
	I = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			(StateData#state.'V(S)'):7, 0:1, (StateData#state.'V(R)'):7, 0:1, Data/binary>>,
	% TX I
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, I}),
	send_iframes(StateData, T).

transmit_uiqueue(StateData) ->
	send_uiframes(StateData, StateData#state.ui_queue),
	StateData#state{ui_queue = []}.

send_uiframes(_, []) -> ok;
send_uiframes(StateData, [Data|T]) ->
	case StateData#state.role of
		network -> CR = 1;
		user -> CR = 0
	end,
	UI = <<(StateData#state.sapi):6, CR:1, 0:1, (StateData#state.tei):7, 1:1,
			2#000:3, 0:1, 2#00:2, 2#11:2, Data/binary>>,
	% TX UI
	gen_fsm:send_event(StateData#state.mux, {'PH', 'DATA', request, UI}),
	send_uiframes(StateData, T).

modulo_subtract(X, Y) when X < Y -> ((X + 128) - Y) rem 128;
modulo_subtract(X, Y) when X >= Y -> X - Y.

modulo_add(X, Y) -> (X + Y) rem 128.

validate_nr(VA, NR, VS) when NR == VA; NR == VS -> true;
validate_nr(VA, NR, VS) when VA < VS, NR > VA, NR < VS -> true;
validate_nr(VA, NR, VS) when VA > VS, NR > VA -> true;
validate_nr(VA, NR, VS) when VA > VS, NR < VS -> true;
validate_nr(_VA, _NR, _VS) -> false.

