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
%%%
%%% @hidden
         
-module(lapd_sap_sup).
-copyright('Copyright (c) 2004 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(supervisor).
-export([init/1]).

init(_Args) ->
	Module = lapd_ce_sup,
	StartFunc = {supervisor, start_link, [Module]},
	ChildSpec = {Module, StartFunc, transient, 4000, supervisor, [Module]},
	{ok, {{simple_one_for_one, 10, 60}, [ChildSpec]}}.

