-module(abis_emulator_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, _Args) ->
	{ok, ServerName} = application:get_env(na_servername),
	{ok, BoardName} = application:get_env(na_boardname),
	{ok, BoardNumber} = application:get_env(na_boardnnumber),
	{ok, BSCLapdId} = application:get_env(bsc_lapdid),
	{ok, BTSLapdId} = application:get_env(bts_lapdid),
	{ok, TEIs} = application:get_env(teis),
	StartArgs = [ServerName, BoardName, BoardNumber, BSCLapdId, BTSLapdId, TEIs],
	supervisor:start_link(abis_emulator_sup, StartArgs).
	
stop(_State) -> ok.
