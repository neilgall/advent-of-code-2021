
ALL := $(shell ls -d day*)

.PHONY: all
all: $(ALL)

.PHONY: clean`
clean:
	find . -name *.run -exec rm -f {} \;
	find . -name *.o -exec rm -f {} \;

%.o: %.c
	gcc -c -o $@ -I ./lib $^

%.run: %.o lib/reader.o
	gcc -o $@ $^

day1: day1/day1.run
	$^

day2: day2/day2.run
	$^

day3: day3/day3.run
	$^
