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
%%% @doc Supervisor for all the connection endpoints.
%%% @end
         
-module(lapd_ce_sup).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(supervisor).
-export([init/1]).

%% broadcast DLE
init([_MUX, _SAPI, _Options] = Args) ->
	DLEStartArgs = [lapd_dle_bcast_fsm, Args, []],
	DLEStartFunc = {gen_fsm, start_link, DLEStartArgs},
	DLEChildSpec = {dle, DLEStartFunc, transient, 4000, worker, [lapd_dle_bcast_fsm]},
	{ok, {{one_for_one, 0, 1}, [DLEChildSpec]}};
%% point-to-point DLE
init([_Self, _MUX, _SAPI, _LME, _Options] = Args) ->
	CMEStartArgs = [lapd_cme_fsm, Args, []],
	CMEStartFunc = {gen_fsm, start_link, CMEStartArgs},
	CMEChildSpec = {cme, CMEStartFunc, transient, 4000, worker, [lapd_cme_fsm]},
	DLEStartArgs = [lapd_dle_p2p_fsm, Args, []],
	DLEStartFunc = {gen_fsm, start_link, DLEStartArgs},
	DLEChildSpec = {dle, DLEStartFunc, transient, 4000, worker, [lapd_dle_p2p_fsm]},
	{ok, {{one_for_one, 0, 1}, [CMEChildSpec, DLEChildSpec]}}.

