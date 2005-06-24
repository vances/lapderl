{application, abis_emulator,
   [{description,    "GSM Abis protocol emulator"},
      {id,           "Abis Emulator"},
      {vsn,          "1.0"},
      {modules,      [abis_emulator, abis_emulator_app, abis_emulator_sup,
                           abis_emulator_na_sup, abis_emulator_link_sup,
                           abis_emulator_tei_sup, 
                           abis_emulator_bsc_fsm, abis_emulator_bts_fsm]},
      {registered,   []},
      {included_applications, []},
      {applications, [kernel, stdlib]},
      {env,          % {na_servers, NASpecs}
                     %    NASpecs = [NASpec]
                     %    NASpec = {NAServerName, NABoardName, NABoardNumber}
                     %    NAServerName = {local,Name} | {global,Name}
                     %    Name = atom()
                     %    NABoardName = string() e.g. "/dev/pri0"
                     %    NABoardNumber = integer()
                     % {abis_links, LinkSpecs}
                     %    LinkSpecs = [LinkSpec]
                     %    LinkSpec = {BSCSpec, BTSSpec, Script, TEIs}
                     %    BSCSpec = {bsc, LapdId, NAServerName}
                     %    BTSSpec = {bts, LapdId, NAServerName}
                     %    LapdId = integer()
                     %    Script = string() e.g. "events.txt"
                     %    TEIs = [integer()]   
                     [{na_servers,
                        [{{local, na}, "/dev/pri0", 0}]},
                     {abis_links,
                        [{{bsc, {local, na}, 0}, {bts, {local, na}, 32},
                             "events.txt", [1, 2, 3, 4]}]}]},
      {mod,          {abis_emulator_app, []}}]}.
