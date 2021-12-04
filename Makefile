
ALL := $(shell ls -d day*)
LIBS := $(subst .c,.o,$(shell ls lib/*.c))

.PHONY: all
all: $(ALL)

.PHONY: clean`
clean:
	find . -name *.run -exec rm -f {} \;
	find . -name *.o -exec rm -f {} \;

%.o: %.c
	gcc -c -o $@ -I ./lib $^

%.run: %.o $(LIBS)
	gcc -o $@ $^


day1: day1/day1.run
	$^

day2: day2/day2.run
	$^

day3: day3/day3.run
	$^

.PHONY: day4
day4:
	(cd day4; scryer-prolog day4 -g main -g halt)

