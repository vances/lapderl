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
%%% @doc Top level supervisor of the LAPD application.
%%%
%%% @hidden
         
-module(lapd_sup).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(supervisor).
-export([init/1]).

init([MUXCallBack, Args, Options]) ->
	LMEStartFunc = {gen_server, start_link, [lapd_lme_server, [], []]},
	LMEChildSpec = {lme, LMEStartFunc, permanent, 4000, worker, [lapd_lme_server]},
	MUXStartFunc = {lapd_mux_fsm, start_link, [MUXCallBack, [Args], Options]},
	MUXChildSpec = {mux, MUXStartFunc, permanent, 4000, worker, [lapd_mux_fsm, MUXCallBack]},
	SAPStartFunc = {supervisor, start_link, [lapd_sap_sup, []]},
	SAPChildSpec = {sap, SAPStartFunc, permanent, infinity, supervisor, [lapd_sap_sup]},
	{ok, {{one_for_one, 10, 60}, [LMEChildSpec, MUXChildSpec, SAPChildSpec]}}.
