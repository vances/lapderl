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
%%% @doc An finite state machine which emulates the Base Station
%%% 		Controller (BSC) side of a contrived Abis protocol session.
%%%
         
-module(abis_emulator_bsc_fsm).
-behaviour(gen_fsm).

-export([init/1, terminate/3]).
-export([link_connection_released/2, awaiting_establish/2,
		link_conection_established/2, awaiting_release/2]).
-export([handle_event/3, handle_info/3, handle_sync_event/4, code_change/4]).

init([LAPD, TEI]) ->
	{LME, _CME, DLE} = lapd:open(LAPD, 0, TEI, [{role, network}]),
	lapd:bind(LME, DLE, self()),
	{ok, link_connection_released, DLE}.

link_connection_released({'DL', 'ESTABLISH', indication, _}, SAP) ->
	{next_state, link_conection_established, SAP};
link_connection_released({'DL', 'ESTABLISH', confirm, _}, SAP) ->
	{next_state, link_conection_established, SAP};
link_connection_released({'DL', 'UNIT DATA', indication, _}, SAP) ->
	{next_state, link_connection_released, SAP};
link_connection_released({'DL', 'RELEASE', indication, _}, SAP) ->
	{next_state, link_connection_released, SAP};
link_connection_released(Event, SAP) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, link_connection_released}]),
	{next_state, link_connection_released, SAP}.

awaiting_establish({'DL', 'ESTABLISH', confirm, _}, SAP) ->
	{next_state, link_conection_established, SAP};
awaiting_establish({'DL', 'RELEASE', indication, _}, SAP) ->
	{next_state, link_conection_released, SAP};
awaiting_establish({'DL', 'UNIT DATA', indication, _}, SAP) ->
	{next_state, awaiting_establish, SAP};
awaiting_establish({'DL', 'ESTABLISH', indication, _}, SAP) ->
	{next_state, awaiting_establish, SAP};
awaiting_establish(Event, SAP) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, awaiting_establish}]),
	{next_state, awaiting_establish, SAP}.

link_conection_established({'DL', 'DATA', indication, _}, SAP) ->
	{next_state, link_connection_established, SAP};
link_conection_established({'DL', 'UNIT DATA', indication, _}, SAP) ->
	{next_state, link_connection_established, SAP};
link_conection_established({'DL', 'ESTABLISH', indication, _}, SAP) ->
	{next_state, link_connection_established, SAP};
link_conection_established({'DL', 'ESTABLISH', confirm, _}, SAP) ->
	{next_state, link_connection_established, SAP};
link_conection_established({'DL', 'RELEASE', indication, _}, SAP) ->
	{next_state, link_connection_released, SAP};
link_conection_established(Event, SAP) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, link_connection_established}]),
	{next_state, link_conection_established, SAP}.

awaiting_release({'DL', 'RELEASE', confirm, _}, SAP) ->
	{next_state, link_connection_released, SAP};
awaiting_release({'DL', 'RELEASE', indication, _}, SAP) ->
	{next_state, awaiting_release, SAP};
awaiting_release({'DL', 'UNIT DATA', indication, _}, SAP) ->
	{next_state, awaiting_release, SAP};
awaiting_release({'DL', 'ESTABLISH', indication, _}, SAP) ->
	{next_state, awaiting_release, SAP};
awaiting_release({'DL', 'ESTABLISH', confirm, _}, SAP) ->
	{next_state, awaiting_release, SAP};
awaiting_release(Event, SAP) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, awaiting_release}]),
	{next_state, awaiting_release, SAP}.

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

