## lapd Makefile

VERSION = 1.1

ERL = erl
ERLC = erlc

.INTERMEDIATE:	%.script %.boot
%.script %.boot:	%.rel ebin/lapd.app all
	${ERLC} -I ./ebin $<

%.tar.gz:	%.script %.boot
	${ERL} -noshell -pa ./ebin -run systools make_tar $* -run init stop

.PHONY:	all
all:	
	cd src && $(MAKE)
	cd examples && $(MAKE)

.PHONY:	doc
doc:	
	cd doc && $(MAKE)

.PHONY:	clean
clean:
	cd src && $(MAKE) $@
	cd examples && $(MAKE) $@
	-rm *.script *.boot

.PHONY:	install
install:	all
	cd ebin && VERSION=$(VERSION) $(MAKE) $@
	cd src && VERSION=$(VERSION) $(MAKE) $@
	cd doc && VERSION=$(VERSION) $(MAKE) $@
	cd examples && VERSION=$(VERSION) $(MAKE) $@

