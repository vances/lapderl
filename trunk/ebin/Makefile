
EMU = beam

.PHONY: all
all:
	echo "Nothing to be done in ebin."

.PHONY:	install
install:
	install -d ${ERL_TOP}/lib/lapd-${VERSION}/ebin
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd.app
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd_sup.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd_sap_sup.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd_ce_sup.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd_lme_server.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd_cme_fsm.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd_dle_p2p_fsm.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd_dle_bcast_fsm.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/ebin lapd_mux_fsm.${EMU}

