
ERLC = erlc -pa ../../ebin
ERLCFLAGS = -W -v
DEBUGFLAGS = +debug_info

INCLUDES = -I $(ERL_TOP)/lib/netaccess-1.1/include

%.beam:%.erl
	${ERLC} ${ERLCFLAGS} ${INCLUDES} ${DEBUGFLAGS} $<

.PHONY:	all
all:	lapd_mux_netaccess_fsm.beam

.PHONY:	clean
clean:
	-rm lapd_mux_netaccess_fsm.beam

