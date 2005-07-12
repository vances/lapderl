{application, lapd_netaccess,
   [{description,    "LAPD Netaccess Adaptation Library"},
      {id,           "LAPD Netaccess"},
      {vsn,          "1.0"},
      {modules,      [lapd_mux_netaccess_fsm]},
      {registered,   []},
      {included_applications, [netaccess]},
      {applications, [kernel, stdlib]}]}.

