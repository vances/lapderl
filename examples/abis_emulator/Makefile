
ERLC = erlc
ERLCFLAGS = -W -v 
DEBUGFLAGS = +debug_info

INCLUDES =

BEAMS = abis_emulator_app.beam abis_emulator_sup.beam \
		abis_emulator_tei_sup.beam abis_emulator_bsc_fsm.beam \
		abis_emulator_bts_fsm.beam

%.beam:%.erl
	${ERLC} ${ERLCFLAGS} ${INCLUDES} ${DEBUGFLAGS} $<

.PHONY:	all
all: ${BEAMS}

.PHONY:	clean
clean:
	-rm ${BEAMS}

