
ERLC = erlc
ERL = erl
ERLCFLAGS = -W -v -o ../ebin
DEBUGFLAGS = +debug_info

INCLUDES =

BEAMS = ../ebin/lapd.beam ../ebin/lapd_sup.beam \
		../ebin/lapd_sap_sup.beam ../ebin/lapd_ce_sup.beam \
		../ebin/lapd_lme_server.beam ../ebin/lapd_cme_fsm.beam \
		../ebin/lapd_dle_fsm.beam ../ebin/lapd_multiplex_fsm.beam

DOCS	= ../doc/html/lapd.html

../ebin/%.beam:%.erl
	${ERLC} ${ERLCFLAGS} ${INCLUDES} ${DEBUGFLAGS} $<

../doc/html/%.html:%.erl
	${ERL} -noshell -run edoc_run file '"$<"' '[{dir,"../doc/html"}]' -s init stop

.PHONY:	all
all: ${BEAMS} ${DOCS}

.PHONY:	clean
clean:
	-rm ${BEAMS} ${DOCS}

