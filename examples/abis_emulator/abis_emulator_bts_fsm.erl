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
%%% @doc An finite state machine which emulates the Base Transceiver
%%% 		Station (BTS) side of a contrived GSM Abis protocol session.
%%%
         
-module(abis_emulator_bts_fsm).
-behaviour(gen_fsm).

-export([init/1, terminate/3]).
-export([init_lapd/2, link_connection_released/2, awaiting_establish/2,
		link_connection_established/2, awaiting_release/2]).
-export([handle_event/3, handle_info/3, handle_sync_event/4, code_change/4]).

-record(state, {sup, tei, sap, events, next}).

init([Sup, TEI]) ->
	{ok, init_lapd, #state{sup = Sup, tei = TEI}, 100}.

init_lapd(timeout, StateData) ->
	Children = supervisor:which_children(StateData#state.sup),
	{value, {bsc, LAPD, _, _}} = lists:keysearch(bsc, 1, Children),
	{ok, File} = application:get_env(file), % TODO:  per {lapd, tei}
	{ok, Events} = file:consult(File),
	{LME, _CME, DLE} = lapd:open(LAPD, 0, StateData#state.tei, []),
	lapd:bind(LME, DLE, self()),
	gen_fsm:send_event(DLE, {'DL', 'ESTABLISH', request, []}),
	{next_state, awaiting_establish, #state{sap = DLE, events = Events}}.

link_connection_released({'DL', 'ESTABLISH', indication, _}, StateData) ->
	{next_state, link_conection_established, StateData};
link_connection_released({'DL', 'ESTABLISH', confirm, _}, StateData) ->
	{next_state, link_conection_established, StateData};
link_connection_released({'DL', 'UNIT DATA', indication, _}, StateData) ->
	{next_state, link_connection_released, StateData};
link_connection_released({'DL', 'RELEASE', indication, _}, StateData) ->
	{next_state, link_connection_released, StateData};
link_connection_released(Event, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, link_connection_released}]),
	{next_state, link_connection_released, StateData}.

awaiting_establish({'DL', 'ESTABLISH', confirm, _}, StateData) ->
	NewStateData = StateData#state{next = 1},
	case next_event(NewStateData) of
		{bts, Timeout, _Type, _PDU} ->
			{next_state, link_conection_established, NewStateData, Timeout};
		{bsc, _, _, _} ->
			{next_state, link_conection_established, NewStateData}
	end;
awaiting_establish({'DL', 'RELEASE', indication, _}, StateData) ->
	{next_state, link_conection_released, StateData};
awaiting_establish({'DL', 'UNIT DATA', indication, _}, StateData) ->
	{next_state, awaiting_establish, StateData};
awaiting_establish({'DL', 'ESTABLISH', indication, _}, StateData) ->
	{next_state, awaiting_establish, StateData};
awaiting_establish(Event, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, awaiting_establish}]),
	{next_state, awaiting_establish, StateData}.

link_conection_established({'DL', UI_I, indication, PDU}, StateData)
		when UI_I == 'DATA'; UI_I == 'UNIT DATA' -> 
	{bsc, _, _, PDU} = next_event(StateData), 
	NewStateData = StateData#state{next = StateData#state.next + 1},
	case next_event(NewStateData) of
		{bts, Timeout, _Type, _PDU} ->
			{next_state, link_connection_established, NewStateData, Timeout};
		{bsc, _, _, _PDU} ->
			{next_state, link_connection_established, NewStateData}
	end;
link_conection_established({'DL', 'ESTABLISH', indication, _}, StateData) ->
	{next_state, link_connection_established, StateData};
link_conection_established({'DL', 'ESTABLISH', confirm, _}, StateData) ->
	{next_state, link_connection_established, StateData};
link_conection_established({'DL', 'RELEASE', indication, _}, StateData) ->
	{next_state, link_connection_released, StateData};
link_conection_established(timeout, StateData) ->
	case next_event(StateData) of
		{bts, _Timeout, i, PDU} ->
			gen_fsm:send_event(StateData#state.sap, {'DL', 'DATA', request, PDU});
		{bts, _Timeout, ui, PDU} ->
			gen_fsm:send_event(StateData#state.sap, {'DL', 'UNIT DATA', request, PDU})
	end,
	NewStateData = StateData#state{next = StateData#state.next + 1},
	case next_event(NewStateData) of
		{bts, Timeout, _Type, _PDU} ->
			{next_state, link_conection_established, NewStateData, Timeout};
		{bsc, _, _, _} ->
			{next_state, link_conection_established, NewStateData}
	end;
link_conection_established(Event, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, link_connection_established}]),
	{next_state, link_conection_established, StateData}.

awaiting_release({'DL', 'RELEASE', confirm, _}, StateData) ->
	{next_state, link_connection_released, StateData};
awaiting_release({'DL', 'RELEASE', indication, _}, StateData) ->
	{next_state, awaiting_release, StateData};
awaiting_release({'DL', 'UNIT DATA', indication, _}, StateData) ->
	{next_state, awaiting_release, StateData};
awaiting_release({'DL', 'ESTABLISH', indication, _}, StateData) ->
	{next_state, awaiting_release, StateData};
awaiting_release({'DL', 'ESTABLISH', confirm, _}, StateData) ->
	{next_state, awaiting_release, StateData};
awaiting_release(Event, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, awaiting_release}]),
	{next_state, awaiting_release, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------

next_event(StateData) ->
	lists:nth(StateData#state.next, StateData#state.events).
