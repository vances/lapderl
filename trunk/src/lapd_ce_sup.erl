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
%%% @doc Supervisor for all the connection endpoints.
%%%
%%% @private
%%%
-module(lapd_ce_sup).
-copyright('Copyright (c) 2004,2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision$').
-behaviour(supervisor).
-export([init/1]).

%% broadcast DLE
init([_MUX, _SAPI, _Options] = Args) ->
	DLEStartArgs = [lapd_dle_bcast_fsm, Args, []],
	DLEStartFunc = {gen_fsm, start_link, DLEStartArgs},
	DLEChildSpec = {dle, DLEStartFunc, transient, 4000, worker, [lapd_dle_bcast_fsm]},
	{ok, {{one_for_one, 0, 1}, [DLEChildSpec]}};
%% point-to-point DLE
init([_MUX, _SAPI, _LME, _Options] = Args) ->
	CMEStartArgs = [lapd_cme_fsm, Args, []],
	CMEStartFunc = {gen_fsm, start_link, CMEStartArgs},
	CMEChildSpec = {cme, CMEStartFunc, transient, 4000, worker, [lapd_cme_fsm]},
	DLEStartArgs = [lapd_dle_p2p_fsm, Args, []],
	DLEStartFunc = {gen_fsm, start_link, DLEStartArgs},
	DLEChildSpec = {dle, DLEStartFunc, transient, 4000, worker, [lapd_dle_p2p_fsm]},
	{ok, {{one_for_one, 0, 1}, [CMEChildSpec, DLEChildSpec]}}.

