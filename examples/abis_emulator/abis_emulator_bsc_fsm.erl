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
%%% @doc An finite state machine which emulates the Base Station 
%%% 		Controller (BSC) side of a contrived GSM Abis protocol session.
%%%
%%% @private
%%%
-module(abis_emulator_bsc_fsm).
-copyright('Copyright (c) 2004,2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision$').
-behaviour(gen_fsm).

-export([init/1, terminate/3]).
-export([init_lapd/2, link_connection_released/2, 
		link_connection_established/2]).
-export([handle_event/3, handle_info/3, handle_sync_event/4, code_change/4]).

-record(state, {link_sup, tei, script, sap, events, next}).

init([LinkSup, TEI, Script]) ->
	{ok, init_lapd, #state{link_sup = LinkSup, tei = TEI, script = Script}, 100}.

init_lapd(timeout, StateData) ->
	Children = supervisor:which_children(StateData#state.link_sup),
	{value, {bsc, LAPD, _, _}} = lists:keysearch(bsc, 1, Children),
	{ok, Events} = file:consult(StateData#state.script),
	{LME, _CME, DLE} = lapd:open(LAPD, 0, StateData#state.tei, [{role, network}]),
	lapd:bind(LME, DLE, self()),
	NewStateData = StateData#state{sap = DLE, events = Events},
	{next_state, link_connection_released, NewStateData}.

link_connection_released({'DL', 'ESTABLISH', indication, _}, StateData) ->
	next_step(StateData#state{next = 1});
link_connection_released({'DL', 'UNIT DATA', indication, _}, StateData) ->
	{next_state, link_connection_released, StateData};
link_connection_released({timeout, _PDU}, StateData) ->
	{next_state, link_connection_released, StateData};
link_connection_released(Event, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, link_connection_released}]),
	{next_state, link_connection_released, StateData}.

link_connection_established({'DL', 'UNIT DATA', indication, _PDU}, StateData) ->
	{next_state, link_connection_established, StateData};
link_connection_established({'DL', 'DATA', indication, PDU}, StateData) ->
	{bts, _, i, PDU} = next_event(StateData),  % verify PDU
	Next = (StateData#state.next rem length(StateData#state.events)) + 1,
	next_step(StateData#state{next = Next});
link_connection_established({'DL', 'RELEASE', indication, _}, StateData) ->
	{next_state, link_connection_released, StateData};
link_connection_established({timeout, PDU}, StateData) ->
	gen_fsm:send_event(StateData#state.sap, {'DL', 'DATA', request, PDU}),
	{next_state, link_connection_established, StateData};
link_connection_established(Event, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {event, Event},
		{state, link_connection_established}]),
	{next_state, link_connection_established, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.
	
handle_info({'DL', 'ESTABLISH', _, _} = Primitive, StateName, StateData) ->
	error_logger:info_report(["BSC Established",
			{sap, StateData#state.sap}, {tei, StateData#state.tei}]),
	?MODULE:StateName(Primitive, StateData);
handle_info({'DL', 'RELEASE', _, _} = Primitive, StateName, StateData) ->
	error_logger:info_report(["BSC Released",
			{sap, StateData#state.sap}, {tei, StateData#state.tei}]),
	?MODULE:StateName(Primitive, StateData);
handle_info({'DL', _, _, _} = Primitive, StateName, StateData) ->
	?MODULE:StateName(Primitive, StateData);
handle_info(Info, StateName, StateData) ->
	error_logger:error_report([{module, ?MODULE}, {line, ?LINE},
		{state, StateName}, {info, Info}]),
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

next_step(StateData) ->
	next_step(StateData, 0).
next_step(StateData, Acc) ->
	case next_event(StateData) of
		{bts, T, ui, _} ->
			Next = (StateData#state.next rem length(StateData#state.events)) + 1,
			next_step(StateData#state{next = Next}, Acc + T);
		{bts, _, i, _} ->
			{next_state, link_connection_established, StateData};
		{bsc, T, _, PDU} ->
			gen_fsm:send_event_after(Acc + T, {timeout, PDU}),
			Next = (StateData#state.next rem length(StateData#state.events)) + 1,
			next_step(StateData#state{next = Next}, Acc + T)
	end.
