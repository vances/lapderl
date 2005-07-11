
.PHONY:	all
all:	
	cd netaccess_adaptation && $(MAKE)
	cd abis_emulator && $(MAKE)

.PHONY:	clean
clean:
	cd netaccess_adaptation && $(MAKE) $@
	cd abis_emulator && $(MAKE) $@

.PHONY:	install
install:	all
	install -d ${ERL_TOP}/lib/lapd-${VERSION}/examples
	cd netaccess_adaptation && $(MAKE) $@
	cd abis_emulator && $(MAKE) $@

