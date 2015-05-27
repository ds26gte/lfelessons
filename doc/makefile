# last change 2015-01-03

txtfiles = $(wildcard [!v]*.txt)

htmlfiles = $(patsubst %.txt,%.html,$(txtfiles))

all: $(htmlfiles)

%.html: %.txt
	txt2page $<

clean:
	rm -fr *.html
