#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"

// ----------------------------------------------
// Model

struct Fish {
	int count[9];
};

void increment_count(struct Fish *fish, int value) {
	assert(0 <= value && value < 9);
	fish->count[value] += 1;
}

int total_fish(struct Fish *fish) {
	int total = 0;
	for (int i = 0; i < 9; ++i) {
		total += fish->count[i];
	}
	return total;
}

void read_input(struct Reader *reader, struct Fish *fish) {
	memset(fish, 0, sizeof(struct Fish));
	char c;
	while ((c = reader->next(reader)) != 0) {
		if (isdigit(c)) {
			increment_count(fish, c - '0');
		}
	}
	reader->delete(reader);
}


// ----------------------------------------------
 
int part1(struct Fish fish) {
	for (int day = 0; day < 80; ++day) {
		int new_fish = fish.count[0];
		for (int i = 0; i < 8; ++i) {
			fish.count[i] = fish.count[i+1];
		}
		fish.count[6] += new_fish;
		fish.count[8] = new_fish;
	}
	return total_fish(&fish);
}


int part2(struct Fish fish) {
	return 0;
}

// ----------------------------------------------
// tests

void test(const char *input, int (*f)(struct Fish), int expect) {
	struct Fish fish;
	read_input(string_reader(input), &fish);
	int actual = f(fish);
	if (actual != expect) {
		fprintf(stderr, "test failed input='%s' expect=%d actual=%d\n",
			input, expect, actual);
		abort();
	}
}


// ----------------------------------------------

static const char test_data[] = 
	"3,4,3,1,2";

int main() {
	test(test_data, part1, 5934);
	test(test_data, part2, 0);

	struct Fish fish;
	read_input(file_reader("day6/input.txt"), &fish);

	printf("Part 1: %d\n", part1(fish));
	printf("Part 2: %d\n", part2(fish));

	return 0;
}