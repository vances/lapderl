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
%%% @doc An example of a layer 1 adaptation module for the netaccess application.
%%% 	<p>This module adapts the netaccess implementation of layer 1 to
%%% 	the LAPD layer 2 implementation.</p>
%%%
         
-module(lapd_mux_netaccess_fsm).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(lapd_mux_fsm).

-include("iisdn.hrl").

-export([init/1, terminate/3]).

% export the gen_fsm state handler call backs
-export([activated/2, deactivated/2]).

% export the gen_fsm common call backs
-export([init/1, handle_event/3, handle_sync_event/4,
		handle_info/3, terminate/3, code_change/4]).

-record(state, {na, port, lapdid}).

%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% usage:
%%       {ok, NA} = netaccess:start_link("/dev/pri0", 0),
%%       {ok, LapdSup} = lapd:start_link(lapd_mux_netaccess_fsm, [NA, 0], []).
%%
init([NA, LapdId]) ->
	{ok, deactivated, #state{na = NA, lapdid = LapdId}}.
                
deactivated({'PH', 'ACTIVATE', request, _}, StateData) ->
	Port = netaccess:open(StateData#state.na),
	L1 = #level1{l1_mode = ?IISDNl1modHDLC},
	L2Parms = #l2_lap_params{mode = ?IISDNl2modDISABLED},
	D = #data_interface{enable = 1, data_channel = StateData#state.lapdid},
	L2 = #level2{par = L2Parms, data_interface = D},
	ProtoData = #ena_proto_data{level1 = L1, level2 = L2},
	% send an L4L3mENABLE_PROTOCOL to activate the channel
	netaccess:enable_protocol(Port, StateData#state.lapdid, ProtoData),
	{next_state, deactivated, StateData#state{port = Port}};
deactivated({_Port, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mPROTOCOL_STATUS ->
	P = iisdn:protocol_stat(L3L4m#l3_to_l4.data),
	case  P#protocol_stat.status of
		?IISDNdsESTABLISHED ->
			gen_fsm:send_event(self(), {'PH', 'ACTIVATE', indication, undefined}),
			{next_state, activated, StateData};
		_ ->
			{next_state, deactivated, StateData}
	end;
deactivated({_Port, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData};
deactivated(_Event, StateData) ->
	{next_state, deactivated, StateData}.

activated({_Port, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData};
activated(_Event, StateData) ->
	{next_state, activated, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% handle a control message received from netaccess
handle_info({Port, {'L3L4m', CtrlBin, _}}, StateName, StateData) 
		when is_binary(CtrlBin), size(CtrlBin) > 0 ->
	L3L4_rec = iisdn:l3_to_l4(CtrlBin),
	StateName({Port, L3L4_rec}, StateData);
%% handle a frame received from netaccess' layer 1
handle_info({_Port, {'L3L4m', _, PDU}}, StateName, StateData) 
		when is_binary(PDU), size(PDU) > 0 ->
	gen_fsm:send_event(self(), {'PH', 'DATA', indication, PDU}),
	{next_state, StateName, StateData};
handle_info(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, StateData) ->
	netaccess:close(StateData#state.port).

code_change(_OldVersion, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

