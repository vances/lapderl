
ERL = erl
ERLC = erlc

.INTERMEDIATE:	%.boot %.script
%.boot %.script:	%.rel
	${ERLC} -I ./ebin $<

%.tar.gz:	%.script %.boot
	${ERL} -noshell -pa ./ebin -run systools make_tar $* -run init stop

.PHONY:	all
all:	src

.PHONY:	src
src:
	cd src && $(MAKE)

.PHONY:	clean
clean:
	cd src && $(MAKE) clean
	-rm *.script *.boot
