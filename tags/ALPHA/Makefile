
ERL = erl

%.boot:ebin/%.rel ebin/%.app
	${ERLC} $<

%.tar.gz:	%.boot
	${ERL} -noshell -run systools make_tar $* -run init stop

.PHONY:	all
all:	src

.PHONY:	src
src:
	cd src && $(MAKE)

.PHONY:	release
release: lapd.tar.gz src sys.config 

.PHONY:	clean
clean:
	cd src && $(MAKE) clean
	-rm lapd.script lapd.tar.gz
