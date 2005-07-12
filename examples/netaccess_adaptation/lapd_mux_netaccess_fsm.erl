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
%%% @author Vance Shipley <vances@motivity.ca>
%%%
%%% @doc An example of a layer 1 adaptation module for the netaccess application.
%%% 	<p>This module adapts the netaccess implementation of layer 1 to
%%% 	the LAPD layer 2 implementation.</p>
%%%
%%% @reference <a href="index.html">The LAPD User's Guide</a>
%%% @reference <a href="http://www.motivity.ca/netaccess">The netaccess User's Guide</a>
%%%
%%% @private
%%%
-module(lapd_mux_netaccess_fsm).
-copyright('Copyright (c) 2004,2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision$').
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
init([NAServerRef, LapdId]) ->
	NA = case NAServerRef of
		Pid when is_pid(Pid) -> Pid;
		{local, Name} -> whereis(Name);
		{global, Name} -> global:whereis_name(Name);
		Name -> Name
	end,
	{ok, deactivated, #state{na = NA, lapdid = LapdId}}.
                
%% enable layer 1
deactivated({'PH', 'ACTIVATE', request, _}, StateData) ->
	Port = netaccess:open(StateData#state.na),
	L1 = #level1{l1_mode = ?IISDNl1modHDLC,
			num_txbuf = 3, num_rxbuf = 5},
	L2Parms = #l2_lap_params{mode = ?IISDNl2modDISABLED},
	D = #data_interface{enable = 1, data_channel = StateData#state.lapdid},
	L2 = #level2{par = L2Parms, data_interface = D},
	ProtoData = #ena_proto_data{level1 = L1, level2 = L2},
	% send an L4L3mENABLE_PROTOCOL to activate the channel
	case netaccess:enable_protocol(Port, StateData#state.lapdid, ProtoData) of
		#protocol_stat{status = ?IISDNdsESTABLISHED} ->
			{next_state, activated, StateData#state{port = Port}};
		_Other ->
			{stop, not_established, StateData#state{port = Port}}
	end.

%% send a frame to layer 1
activated({'PH', 'DATA', request, PDU}, StateData) when is_binary(PDU) ->
	netaccess:send(StateData#state.port, PDU),
	{next_state, activated, StateData};
activated({_Port, L3L4m}, StateData) when is_record(L3L4m, l3_to_l4),
		L3L4m#l3_to_l4.msgtype == ?L3L4mERROR ->
	Reason = iisdn:error_code(L3L4m#l3_to_l4.data),
	{stop, Reason, StateData};
activated(_Other, StateData) ->
	{next_state, activated, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{stop, StateName, StateData}.

%% handle a control message received from netaccess
handle_info({Port, {'L3L4m', CtrlBin, _}}, StateName, StateData) 
		when is_binary(CtrlBin), size(CtrlBin) > 0 ->
	L3L4_rec = iisdn:l3_to_l4(CtrlBin),
	?MODULE:StateName({Port, L3L4_rec}, StateData);
%% handle a frame received from netaccess' layer 1
handle_info({_Port, {'L3L4m', _, PDU}}, StateName, StateData) 
		when is_binary(PDU), size(PDU) > 0 ->
	gen_fsm:send_event(self(), {'PH', 'DATA', indication, PDU}),
	{next_state, StateName, StateData};
handle_info({'EXIT', Port, Reason}, _StateName, StateData) 
		when is_port(Port) ->
	{stop, Reason, StateData}.

terminate(Reason, _StateName, StateData) 
		when is_port(StateData#state.port) ->
	catch netaccess:close(StateData#state.port),
	Reason;
terminate(Reason, _StateName, _StateData) ->
	Reason.

code_change(_OldVersion, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

