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
%%% @doc Top level supervisor of the LAPD application.
%%%
%%% @private
%%%
-module(lapd_sup).
-copyright('Copyright (c) 2004,2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision$').
-behaviour(supervisor).
-export([init/1]).

init([MUXCallBack, Args, Options]) ->
	{LMEOptions, MUXOptions} = split_options(Options),
	LMEStartFunc = {gen_server, start_link, [lapd_lme_server, LMEOptions, []]},
	LMEChildSpec = {lme, LMEStartFunc, permanent, 4000, worker, [lapd_lme_server]},
	MUXStartFunc = {lapd_mux_fsm, start_link, [MUXCallBack, Args, MUXOptions]},
	MUXChildSpec = {mux, MUXStartFunc, permanent, 4000, worker, [lapd_mux_fsm, MUXCallBack]},
	SAPStartFunc = {supervisor, start_link, [lapd_sap_sup, []]},
	SAPChildSpec = {sap, SAPStartFunc, permanent, infinity, supervisor, [lapd_sap_sup]},
	{ok, {{one_for_one, 10, 60}, [LMEChildSpec, MUXChildSpec, SAPChildSpec]}}.

%% (Options::list()) -> Result
%% 	Result = {LapdOptions, MuxOptions}
%% 	LapdOptions = [tuple()]
%% 	MuxOptions  = [tuple()]
%%
%% @doc Seperate lapd options from callback (gen_fsm) options.
%% @hidden
%%
split_options(Options) ->
	LapdDefaults = [{k,7}, {n200,3}, {n201,260}, {n202,3}, {role,user},
 			{t200,1000}, {t201,1000}, {t202,2000}, {t203,10000}], % keep sorted!
	split_options(lists:keysort(1, Options), LapdDefaults, []).
split_options(Options, [{Key, Default}|T], Acc) ->
	case lists:keysearch(Key, 1, Options) of
		{value, {Key, NewValue}} ->
			split_options(lists:keydelete(Key, 1, Options), T, Acc ++ [{Key, NewValue}]);
		false ->
			split_options(Options, T, Acc ++ [{Key, Default}])
	end;
split_options(Options, [], Acc) ->
	{Acc, Options}.
	
