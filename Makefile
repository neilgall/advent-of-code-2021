
ALL := $(shell ls -d day*)
LIBS := $(subst .c,.o,$(shell ls lib/*.c))

.PHONY: all
all: $(ALL)

.PHONY: clean`
clean:
	find . -name *.run -exec rm -f {} \;
	find . -name *.o -exec rm -f {} \;
	find . -name .venv -exec rm -rf {} \;


# C build/run rules

%.o: %.c
	gcc -g -c -o $@ -I ./lib $^

%.run: %.o $(LIBS)
	gcc -o $@ $^


# Python build/run rules

%/.venv:
	cd `dirname $@` && virtualenv --python=python3 .venv
	$@/bin/pip install -r `dirname $@`/requirements.txt


# Individual day targets

day1: day1/day1.run
	$^

day2: day2/day2.run
	$^

day3: day3/day3.run
	$^

day4: day4/day4.pl
	(cd day4; scryer-prolog day4 -g main -g halt)

.PHONY: day5
day5: day5/.venv day5/day5.py
	(cd $@; .venv/bin/pytest -v $@.py; .venv/bin/python $@.py)

day6: day6/day6.run
	$^
