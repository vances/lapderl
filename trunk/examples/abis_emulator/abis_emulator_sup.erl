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
%%% @doc Top level supervisor for the abis_emulator application.
%%%
%%% @private
%%%
-module(abis_emulator_sup).
-copyright('Copyright (c) 2004,2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision$').
-behaviour(supervisor).
-export([init/1]).

init([NAServerName, BoardName, BoardNumber, BSCLapdId, BTSLapdId, TEIs]) ->
	NAChildSpec = init_na(NAServerName, BoardName, BoardNumber),
	BSCChildSpec = init_lapd(bsc, NAServerName, BSCLapdId),
	BTSChildSpec = init_lapd(bts, NAServerName, BTSLapdId),
	TEIChildSpecs = init_tei(TEIs, []),
	{ok, {{one_for_one, 0, 1}, [NAChildSpec, BSCChildSpec, BTSChildSpec] ++ TEIChildSpecs}}.

init_na(NAServerName, BoardName, BoardNumber) ->
	StartArgs = [NAServerName, BoardName, BoardNumber],
	StartFunc = {netaccess, start_link, StartArgs},
	{na, StartFunc, permanent, 4000, worker, [netaccess]}.

init_lapd(Mode, NA, LapdId) ->
	StartArgs = [lapd_mux_netaccess_fsm, [NA, LapdId], []],
	StartFunc = {lapd, start_link, StartArgs},
	{Mode, StartFunc, permanent, 4000, worker, [lapd, lapd_mux_netaccess_fsm]}.

init_tei([TEI|T], ChildSpecs) ->
	StartFunc = {supervisor, start_link, [abis_emulator_tei_sup, [self(), TEI]]},
	ChildSpec = {TEI, StartFunc, permanent, infinity, supervisor, [abis_emulator_tei_sup]},
	init_tei(T, ChildSpecs ++ [ChildSpec]);
init_tei([], ChildSpecs) ->
	ChildSpecs.

