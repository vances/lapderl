-module(abis_emulator_tei_sup).
-behaviour(supervisor).
-export([init/1]).

init([Sup, TEI]) ->
	BSCStartFunc = {gen_fsm, start_link, [abis_emulator_bsc_fsm, [Sup, TEI], []]},
	BSCChildSpec = {bsc, BSCStartFunc, permanent, 4000, worker, [abis_emulator_bsc_fsm]},
	BTSStartFunc = {gen_fsm, start_link, [abis_emulator_bts_fsm, [Sup, TEI], []]},
	BTSChildSpec = {bts, BTSStartFunc, permanent, 4000, worker, [abis_emulator_bts_fsm]},
	{ok, {{one_for_all, 10, 60}, [BSCChildSpec, BTSChildSpec]}}.

