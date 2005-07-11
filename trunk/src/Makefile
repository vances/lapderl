
EMU = beam

ERLC = erlc -b ${EMU}
ERL = erl
ERLCFLAGS = -W -v -o ../ebin
DEBUGFLAGS = +debug_info

INCLUDES =

BEAMS = ../ebin/lapd.${EMU} ../ebin/lapd_sup.${EMU} \
		../ebin/lapd_sap_sup.${EMU} ../ebin/lapd_ce_sup.${EMU} \
		../ebin/lapd_lme_server.${EMU} ../ebin/lapd_cme_fsm.${EMU} \
		../ebin/lapd_dle_p2p_fsm.${EMU} ../ebin/lapd_dle_bcast_fsm.${EMU} \
		../ebin/lapd_mux_fsm.${EMU}

DOCS	= ../doc/html/lapd.html ../doc/html/lapd_mux_fsm.html

../ebin/%.${EMU}:%.erl
	${ERLC} ${ERLCFLAGS} ${INCLUDES} ${DEBUGFLAGS} $<

../doc/html/%.html:%.erl
	${ERL} -noshell -run edoc_run file '"$<"' '[{dir,"../doc/html"}]' -s init stop

.PHONY:	all
all: ${BEAMS} doc

.PHONY: doc
doc:	$(DOCS)

.PHONY:	clean
clean:
	-rm ${BEAMS} ${DOCS}

.PHONY:	install
install:	all
	install -d ${ERL_TOP}/lib/lapd-${VERSION}/src
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd_sup.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd_sap_sup.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd_ce_sup.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd_lme_server.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd_cme_fsm.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd_dle_p2p_fsm.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd_dle_bcast_fsm.erl
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/src lapd_mux_fsm.erl

