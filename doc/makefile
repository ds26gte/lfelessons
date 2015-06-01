# last change 2015-06-01

txtfiles = $(wildcard [!v]*.txt)

htmlfiles = $(patsubst %.txt,%.html,$(txtfiles))

all: $(htmlfiles)

%.html: %.txt
	txt2page -c ../default.css $<

clean:
	rm -fr *.html
