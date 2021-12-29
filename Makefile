
ALL := $(shell ls -d day*)
LIBS := $(subst .c,.o,$(shell ls lib/*.c))

.PHONY: all
all: $(ALL)

.PHONY: clean`
clean:
	-find . -name *.run -exec rm -f {} \;
	-find . -name *.o -exec rm -f {} \;
	-find . -name .venv -exec rm -rf {} \;
	-for day in `find . -name Cargo.toml`; do \
		cd `dirname $$day` && cargo clean; \
	 done


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
	(cd day4; swipl -f -q day4.pl)

.PHONY: day5
day5: day5/.venv day5/day5.py
	(cd $@; .venv/bin/pytest -v $@.py; .venv/bin/python $@.py)

day6: day6/day6.run
	$^

.PHONY: day7
day7: day7/day7.pl
	(cd day7; swipl -f -q day7.pl)

.PHONY: day8
day8: day8/day8.hs
	(cd day8; swipl -f -q day8.pl)

day9: day9/day9.run
	$^

.PHONY: day10
day10: day10/day10.py
	(cd $@; python3 $@.py)

day11: day11/day11.run
	$^

day12/input.pl: day12/input.txt day12/make_rules.py
	python3 day12/make_rules.py <$^ >$@

.PHONY: day12
day12: day12/day12.pl day12/lib.pl day12/input.pl
	(cd $@; \
		swipl -g run_tests -t halt lib.plt; \
		swipl -f -q -g main day12.pl)

.PHONY: day13
day13: day13/day13.py
	(cd $@; python3 $@.py)

day14: day14/day14.run
	$^

.PHONY: day15
day15:
	(cd day15; cargo test && cargo run --release)

.PHONY: day16
day16: day16/.venv day16/day16.py
	(cd $@; .venv/bin/pytest -v $@.py; .venv/bin/python $@.py)

.PHONY: day17
day17: day17/day17.pl
	(cd $@; \
		swipl -g run_tests -t halt day17.pl; \
		swipl -f -q -g main day17.pl)
	
.PHONY: day18
day18: day18/.venv day18/day18.py
	(cd $@; .venv/bin/pytest -s -v $@.py; .venv/bin/python $@.py)

.PHONY: day19
day19:
	(cd day19; cargo test && cargo run --release)

day20: day20/day20.run
	$^

.PHONY: day21/
day21: day21/day21.py
	python3 $^
