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
%%% @doc Q.921 LAPD Layer Management Entity (LME) procedures.
%%%
%%% @reference ITU-T Q.921 ISDN user-network interface - Data link layer specification 
%%%
%%% @reference ETSI ETS 300 125 Integrated Services Digital Network (ISDN);
%%% 	User-network interface data link layer specification;
%%% 	Application of CCITT Recommendations Q.920/I.440 and Q.921/I.441 
%%%
%%% @private
         
-module(lapd_lme_server).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-record(state, {sapsup, physap, saps = []}).
-record(sap, {dle, cme, sapi, sup, usap}).

init([PhySAP]) ->
	process_flag(trap_exit, true),
	{ok, #state{physap = PhySAP}}.

handle_call({'SMAP', 'OPEN', request, {SapSup, SAPI, Options}}, {Pid, _Tag}, State) ->
	case supervisor:start_child(SapSup, [[SapSup, State#state.physap, SAPI, self(), Options]]) of
		{ok, CeSup} ->
			link(CeSup),
			Children = supervisor:which_children(CeSup),
			{value, {cme, CME, _, _}} = lists:keysearch(cme, 1, Children),
			{value, {dle, DLE, _, _}} = lists:keysearch(dle, 1, Children),
			SapRec = #sap{sapi = SAPI, cme = CME, dle = DLE, sup = CeSup},
			NewSaps = sap_insert(SapRec, State#state.saps),
			NewState = State#state{sapsup = SapSup, saps = NewSaps},
			{reply, {self(), CME, DLE}, NewState};
		{error, Reason} ->
			exit(Pid, Reason),
			{noreply, State}
	end;
handle_call({'SMAP', 'BIND', request, {DLE, USAP}}, {Pid, _Tag}, State) ->
	case catch sap_search({dle, DLE}, State#state.saps) of
		SapRec when is_record(SapRec, sap) ->
			CME = SapRec#sap.cme,
			gen_fsm:send_event(DLE, {'MDL', 'BIND', request, {CME, DLE, USAP}}),
			gen_fsm:send_event(CME, {'MDL', 'BIND', request, {CME, DLE, USAP}}),
			NewSaps = sap_update(SapRec#sap{usap = USAP}, State#state.saps),
			NewState = State#state{saps = NewSaps},
			{reply, ok, NewState};
		_ ->
			exit(Pid, badarg),
			{noreply, State}
	end;
handle_call({'SMAP', 'CLOSE', request, DLE}, {Pid, _Tag}, State) ->
	case catch sap_search({dle, DLE}, State#state.saps) of
		SapRec when is_record(SapRec, sap) ->
			exit(SapRec#sap.sup, shutdown),
			{reply, ok, State};
		_ ->
			exit(Pid, badarg),
			{noreply, State}
	end;
handle_call(Request, _From, State) ->
	error_logger:info_report([{module, ?MODULE}, {line, ?LINE},
			{message, Request}, {from, from}]),
	{noreply, State}.

handle_cast(Request, State) ->
	error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {message, Request}]),
	{noreply, State}.
	
handle_info({'EXIT', Pid, _Reason}, State) ->
	SapRec = sap_search({sup, Pid}, State#state.saps),
	NewState = State#state{saps = sap_delete(SapRec, State#state.saps)},
	{noreply, NewState};
handle_info(Info, State) ->
	error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {message, Info}]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------

sap_search({dle, DLE}, [SapRec | _]) when SapRec#sap.dle == DLE ->
	SapRec;
sap_search({sup, Sup}, [SapRec | _]) when SapRec#sap.sup == Sup ->
	SapRec;
sap_search(Key, [_ | T]) ->
	sap_search(Key, T).

sap_update(SapRec, SapRecList) ->
	sap_update(SapRec, SapRecList, []).
sap_update(SapRec, [H | T], Acc) when H#sap.dle == SapRec#sap.dle ->
	[Acc] ++ [SapRec] ++ T;
sap_update(SapRec, [H | T], Acc) ->
	sap_update(SapRec, T, Acc ++ [H]).

sap_insert(SapRec, SapRecList) ->
	SapRecList ++ [SapRec].

sap_delete(SapRec, SapRecList) ->
	sap_delete(SapRec, SapRecList, []).
sap_delete(SapRec, [H | T], Acc) when H#sap.dle == SapRec#sap.dle ->
	Acc ++ T;
sap_delete(SapRec, [H | T], Acc) ->
	sap_delete(SapRec, T, Acc ++ [H]).

