-module(abis_emulator_tei_sup).
-behaviour(supervisor).
-export([init/1]).

init([Sup, TEI]) ->
	Children = supervisor:which_children(Sup),
	{value, {bsc, BSC, _, _}} = lists:keysearch(bsc, 1, Children),
	{value, {bts, BTS, _, _}} = lists:keysearch(bts, 1, Children),
	BSCStartFunc = {gen_fsm, start_link, [abis_emulator_bsc_fsm, [BSC, TEI], []]},
	BSCChildSpec = {bsc, BSCStartFunc, permanent, 4000, worker, [abis_emulator_bsc_fsm]},
	BTSStartFunc = {gen_fsm, start_link, [abis_emulator_bts_fsm, [BTS, TEI], []]},
	BTSChildSpec = {bts, BTSStartFunc, permanent, 4000, worker, [abis_emulator_bts_fsm]},
	{ok, {{all_for_one, 10, 60}, [BSCChildSpec, BTSChildSpec]}}.

