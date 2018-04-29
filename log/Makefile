# Projekt: Rubikova kostka
# Login: xorman00
# Meno: Adam Ormandy

all: flp18-log

flp18-log: flp18-log.pl
	swipl -q -g start -o flp18-log -c flp18-log.pl

.PHONY: clean
clean:
	rm flp18-log

.PHONY: zip
zip:
	zip flp18-log.pl tests README.md Makefile
