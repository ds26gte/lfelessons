# last change 2014-11-26

default: html

all: html beam dist

###

html: doc/index.html

txtfiles = $(wildcard doc/*.txt doc/*.css doc/.lispwords)

doc/index.html: $(txtfiles)
	make -C doc

###

lfefiles = $(wildcard src/*.lfe)

erlfiles = $(wildcard src/*.erl)

beamfiles = $(patsubst src/%.lfe,ebin/%.beam,$(lfefiles)) \
$(patsubst src/%.erl,ebin/%.beam,$(erlfiles))

beam: beamdir $(beamfiles)

beamdir:
	@if test ! -d ebin; then mkdir ebin; fi

ebin/%.beam: src/%.lfe
	lfec -o ebin $<

ebin/%.beam: src/%.erl
	erlc -o ebin $<

###

test: beam
	@erl -noshell -s eunit test testall -s erlang halt

###

allfiles = $(lfefiles) $(erlfiles) $(txtfiles) \
doc/history doc/makefile makefile manifest

diry = $(shell basename $$(pwd))

dist: $(diry).tar.bz2

$(diry).tar.bz2: $(allfiles)
	cd ..; \
	tar cf $(diry).tar $(diry)/makefile; \
	for f in $(allfiles); do \
	tar uf $(diry).tar $(diry)/$$f; \
	done; \
	bzip2 -f $(diry).tar; \
	mv $(diry).tar.bz2 $(diry)

###

clean:
	rm -fr ebin *.tar.bz2 doc/*.html
