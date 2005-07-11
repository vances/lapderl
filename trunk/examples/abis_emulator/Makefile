
EMU = beam

ERLC = erlc
ERLCFLAGS = -W -v 
DEBUGFLAGS = +debug_info

INCLUDES =

BEAMS = abis_emulator_app.${EMU} \
		abis_emulator_sup.${EMU} abis_emulator_na_sup.${EMU} \
		abis_emulator_link_sup.${EMU} abis_emulator_tei_sup.${EMU} \
		abis_emulator_bsc_fsm.${EMU} abis_emulator_bts_fsm.${EMU}

%.${EMU}:%.erl
	${ERLC} ${ERLCFLAGS} ${INCLUDES} ${DEBUGFLAGS} $<

.PHONY:	all
all: ${BEAMS}

.PHONY:	clean
clean:
	-rm ${BEAMS}

.PHONY:	install
install:	all
	install -d ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator.app
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_app.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_sup.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_na_sup.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_link_sup.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_tei_sup.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_bsc_fsm.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_bts_fsm.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_app.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_sup.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_na_sup.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_link_sup.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_tei_sup.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_bsc_fsm.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/abis_emulator abis_emulator_bts_fsm.erl

