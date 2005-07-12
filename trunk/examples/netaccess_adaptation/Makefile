
EMU = beam

ERLC = erlc -b ${EMU} -pa ../../ebin
ERLCFLAGS = -W -v
DEBUGFLAGS = +debug_info

INCLUDES = -I $(ERL_TOP)/lib/netaccess-1.1/include

%.${EMU}:%.erl
	${ERLC} ${ERLCFLAGS} ${INCLUDES} ${DEBUGFLAGS} $<

.PHONY:	all
all:	lapd_mux_netaccess_fsm.${EMU}

.PHONY:	clean
clean:
	-rm lapd_mux_netaccess_fsm.${EMU}

.PHONY:	install
install:	all
	install -d ${ERL_TOP}/lib/lapd-${VERSION}/examples/netaccess_adaptation
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/netaccess_adaptation lapd_netaccess.app
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/netaccess_adaptation lapd_mux_netaccess_fsm.${EMU}
	install -f ${ERL_TOP}/lib/lapd-${VERSION}/examples/netaccess_adaptation lapd_mux_netaccess_fsm.erl

