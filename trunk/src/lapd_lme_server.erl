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
-copyright('Copyright (c) 2004,2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-record(state, {sapsup, mux, bdle, options, saps = []}).
-record(sap, {dle, cme, sapi, tei, usap}).

init(Options) ->
	process_flag(trap_exit, true),
	{ok, #state{options = lists:keysort(1, Options)}}.

%% handle a lapd:open/4 call for a broadcast DLE
handle_call({'SMAP', 'OPEN', request, {SapSup, SAPI, 127, Options}}, {Pid, _Tag}, State) ->
	EndPointOptions = merge(Options, State#state.options),
	case supervisor:start_child(SapSup, [[State#state.mux, SAPI, EndPointOptions]]) of
		{ok, CeSup} ->
			Children = supervisor:which_children(CeSup),
			{value, {dle, DLE, _, _}} = lists:keysearch(dle, 1, Children),
			gen_fsm:send_all_state_event(State#state.mux, {open, {SAPI, 127, DLE}}),
			NewState = State#state{sapsup = SapSup},
			{reply, {self(), undefined, DLE}, NewState};
		{error, Reason} ->
			exit(Pid, Reason),
			{noreply, State}
	end;
%% handle a lapd:open/4 call for a point-to-point DLE
handle_call({'SMAP', 'OPEN', request, {SapSup, SAPI, TEI, Options}}, {Pid, _Tag}, State) ->
	EndPointOptions = merge(Options, State#state.options),
	case supervisor:start_child(SapSup, [[State#state.mux, SAPI, self(), EndPointOptions]]) of
		{ok, CeSup} ->
			Children = supervisor:which_children(CeSup),
			{value, {cme, CME, _, _}} = lists:keysearch(cme, 1, Children),
			{value, {dle, DLE, _, _}} = lists:keysearch(dle, 1, Children),
			gen_fsm:send_event(CME, {dle, DLE}),
			gen_fsm:send_event(DLE, {cme, CME}),
			gen_fsm:send_all_state_event(State#state.mux, {open, {SAPI, TEI, DLE}}),
			case TEI of
				X when is_integer(X), X >= 0, X =< 63 ->
					gen_fsm:send_event(CME, {'MDL', 'ASSIGN', request, {TEI, DLE}}),
					SapRec = #sap{sapi = SAPI, tei = TEI, cme = CME, dle = DLE};
				_ ->
					SapRec = #sap{sapi = SAPI, cme = CME, dle = DLE}
			end,
			link(DLE),
			NewSaps = sap_insert(SapRec, State#state.saps),
			NewState = State#state{sapsup = SapSup, saps = NewSaps},
			{reply, {self(), CME, DLE}, NewState};
		{error, Reason} ->
			exit(Pid, Reason),
			{noreply, State}
	end;
handle_call({'SMAP', 'BIND', request, {DLE, USAP}}, _From, State) ->
	case catch sap_search({dle, DLE}, State#state.saps) of
		% point-to-point DLE
		SapRec when is_record(SapRec, sap) ->
			gen_fsm:send_all_state_event(DLE, {'MDL', 'BIND', request, USAP}),
			NewSaps = sap_update(SapRec#sap{usap = USAP}, State#state.saps),
			NewState = State#state{saps = NewSaps},
			{reply, ok, NewState};
		_ ->
			% broadcast DLE
			gen_fsm:send_all_state_event(DLE, {'MDL', 'BIND', request, USAP}),
			{reply, ok, State}
	end;
%% complete initialization of the lapd layer
handle_call({activate, SapSup, MUX}, {Pid, _Tag}, State) ->
	{value, {role, Role}} = lists:keysearch(role, 1, State#state.options),
	Options = [{role, Role}],
	case supervisor:start_child(SapSup, [[MUX, 63, Options]]) of
		{ok, CeSup} ->
			Children = supervisor:which_children(CeSup),
			{value, {dle, BDLE, _, _}} = lists:keysearch(dle, 1, Children),
			gen_fsm:send_all_state_event(MUX, {open, {63, 127, BDLE}}),
			gen_fsm:send_event(MUX, {'PH', 'ACTIVATE', request, undefined}),
			NewState = State#state{sapsup = SapSup, mux = MUX, bdle = BDLE},
			{reply, ok, NewState};
		{error, Reason} ->
			exit(Pid, Reason),
			{stop, Reason, State}
	end;
handle_call(Request, _From, State) ->
	error_logger:info_report([{module, ?MODULE}, {line, ?LINE},
			{lme, self()}, {message, Request}, {from, from}]),
	{noreply, State}.

handle_cast(Request, State) ->
	error_logger:info_report([{module, ?MODULE}, {line, ?LINE},
			{lme, self()}, {message, Request}]),
	{noreply, State}.
	
handle_info({'EXIT', Pid, _Reason}, State) ->
	SapRec = sap_search({dle, Pid}, State#state.saps),
	NewState = State#state{saps = sap_delete(SapRec, State#state.saps)},
	{noreply, NewState};
handle_info(Info, State) ->
	error_logger:info_report([{module, ?MODULE}, {line, ?LINE},
			{lme, self()}, {message, Info}]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------

sap_search({dle, DLE}, [SapRec | _]) when SapRec#sap.dle == DLE ->
	SapRec;
sap_search(Key, [_ | T]) ->
	sap_search(Key, T).

sap_update(SapRec, SapRecList) ->
	sap_update(SapRec, SapRecList, []).
sap_update(SapRec, [H | T], Acc) when H#sap.dle == SapRec#sap.dle ->
	Acc ++ [SapRec] ++ T;
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

merge([{Key,Value}|T], Options) ->
	merge(T, lists:keyreplace(Key, 1, Options, {Key, Value}));
merge([], Options) ->
	Options.
