
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

.PHONY:	clean
clean:
	cd src && $(MAKE) clean
	cd examples && $(MAKE) clean
	-rm *.script *.boot
