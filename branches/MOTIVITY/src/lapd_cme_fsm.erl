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
%%% @doc A finite state machine implementng the Q.921 LAPD 
%%% 	Communication Management Entity (CME) procedures.
%%%
%%%
%%% @reference ITU-T Q.921 ISDN user-network interface - Data link layer specification 
%%% 	Annex II; Occurrence of MDL-ERROR indication within the basic states 
%%%	and actions to be taken by the management entity 
%%%
%%% @reference ETSI ETS 300 125 Integrated Services Digital Network (ISDN);
%%% 	User-network interface data link layer specification;
%%% 	Application of CCITT Recommendations Q.920/I.440 and Q.921/I.441 
%%%
%%% @private
         
-module(lapd_cme_fsm).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(gen_fsm).

-export([init/1, terminate/3]).
-export([await_bind/2, active/2]).
-export([handle_event/3, handle_info/3, handle_sync_event/4, code_change/4]).

-record(state, {lme, dle, sapi, tei, role}).

init([_Sup, _PhySAP, SAPI, LME, Options]) ->
	Role = case lists:keysearch(role, 1, Options) of
		{value, Value} -> Value;
		_ -> user
	end,
	{ok, await_bind, #state{lme = LME, sapi = SAPI, role = Role}}.

await_bind({'MDL', 'BIND', request, {_CME, DLE, _USAP}}, StateData) ->
	{next_state, active, StateData#state{dle = DLE}}.

%% ref:  Q.921 Annex II Table II.1/Q921 - Management Entity Actions for MDL-Error-Indications
active({'MDL', 'ERROR', indication, Error}, StateData)
		when StateData#state.role == network, Error == 'C';
		StateData#state.role == network, Error == 'D';
		StateData#state.role == network, Error == 'G';
		StateData#state.role == network, Error == 'H' ->
	log(Error, StateData),
	gen_fsm:send_event(StateData#state.lme,
			{'MDL', 'TEI CHECK', request, {StateData#state.tei, StateData#state.dle}}),
	{next_state, active, StateData};
active({'MDL', 'ERROR', indication, Error}, StateData)
		when Error == 'C'; Error == 'D'; Error == 'G'; Error == 'H' ->
	log(Error, StateData),
	gen_fsm:send_event(StateData#state.lme,
			{'MDL', 'TEI VERIFY', request, {StateData#state.tei, StateData#state.dle}}),
	{next_state, active, StateData};
active({'MDL', 'ERROR', indication, Error}, StateData) ->
	log(Error, StateData),
	{next_state, active, StateData};
active({'MDL', 'TEI CHECK', response, free}, StateData) ->
	{next_state, active, StateData#state{tei = undefined}};
active({'MDL', 'TEI CHECK', response, single}, StateData) ->
	{next_state, active, StateData};
active({'MDL', 'TEI CHECK', response, multiple}, StateData) ->
	gen_fsm:send_event(StateData#state.lme,
		{'MDL', 'TEI REMOVAL', request, {StateData#state.tei, StateData#state.dle}}),
	{next_state, active, StateData};
active({'MDL', 'ASSIGN', request, {TEI, CES}}, StateData)
		when CES == StateData#state.dle ->
	gen_fsm:send_event(StateData#state.dle, {'MDL', 'ASSIGN', request, {TEI, CES}}),
	{next_state, active, StateData#state{tei = TEI}};
active(Event, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {line, ?LINE}, {message, Event}]),
	{next_state, active, StateData}.


handle_event(Event, StateName, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {line, ?LINE}, {message, Event}]),
	{next_state, StateName, StateData}.
	
handle_sync_event(Event, _From, StateName, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {line, ?LINE}, {message, Event}]),
	{next_state, StateName, StateData}.
	
handle_info(Info, StateName, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {line, ?LINE}, {message, Info}]),
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------

log(Error, StateData) ->
	ErrorString = case Error of
		'A' ->	"Supervisory (F=1)";
		'B' ->	"DM (F=1)";
		'C' ->	"UA (F=1)";
		'D' ->	"UA (F=0)";
		'E' ->	"Receipt of DM response (F=0)";
		'F' ->	"SABME";
		'G' ->	"SABME";
		'H' ->	"DIS";
		'I' ->	"Status enquiry";
		'J' ->	"N(R) error";
		'K' ->	"Receipt of FRMR response";
		'L' ->	"Receipt of undefined frame";
		'M' ->	"Receipt of I field not permitted";
		'N' ->	"Receipt of frame with wrong size";
		'O' ->	"N201 error";
		Other ->	Other
	end,
	error_logger:error_report([{'MDL-ERROR', ErrorString},
			{sapi, StateData#state.sapi}, {tei, StateData#state.tei}]).

