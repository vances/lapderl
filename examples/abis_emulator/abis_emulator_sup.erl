-module(abis_emulator_sup).
-behaviour(supervisor).
-export([init/1]).

init([NAServerName, BoardName, BoardNumber, BSCLapdId, BTSLapdId, TEIs]) ->
	NAChildSpec = init_na(NAServerName, BoardName, BoardNumber),
	BSCChildSpec = init_lapd(bsc, NAServerName, BSCLapdId),
	BTSChildSpec = init_lapd(bts, NAServerName, BTSLapdId),
	TEIChildSpecs = init_tei(TEIs, []),
	{ok, {{one_for_one, 10, 60}, [NAChildSpec, BSCChildSpec, BTSChildSpec] ++ TEIChildSpecs}}.

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

