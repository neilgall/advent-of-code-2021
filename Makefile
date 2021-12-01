
ALL := $(shell ls -d day*)

.PHONY: all
all: $(ALL)

.PHONY: clean`
clean:
	find . -name *.run -exec rm -f {} \;

day1: day1/day1.run
	$^

%.run: %.c
	gcc -o $@ $^
