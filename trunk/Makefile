
ERL = erl
ERLC = erlc

%.boot:ebin/%.rel ebin/lapd.app all
	${ERLC} -I ./ebin $<

%.tar.gz:	%.boot
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
