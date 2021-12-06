#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"

// ----------------------------------------------
// Model

typedef unsigned long count_t;

struct Fish {
	count_t count[9];
};

void increment_count(struct Fish *fish, count_t value) {
	assert(0 <= value && value < 9);
	fish->count[value] += 1;
}

count_t total_fish(struct Fish *fish) {
	count_t total = 0;
	for (size_t i = 0; i < 9; ++i) {
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

void iterate(struct Fish *fish) {
	count_t new_fish = fish->count[0];
	for (size_t i = 0; i < 8; ++i) {
		fish->count[i] = fish->count[i+1];
	}
	fish->count[6] += new_fish;
	fish->count[8] = new_fish;	
}
 
count_t part1(struct Fish fish) {
	for (size_t day = 0; day < 80; ++day) {
		iterate(&fish);
	}
	return total_fish(&fish);
}


count_t part2(struct Fish fish) {
	for (size_t day = 0; day < 256; ++day) {
		iterate(&fish);
	}
	return total_fish(&fish);
}

// ----------------------------------------------
// tests

void test(const char *input, count_t (*f)(struct Fish), count_t expect) {
	struct Fish fish;
	read_input(string_reader(input), &fish);
	count_t actual = f(fish);
	if (actual != expect) {
		fprintf(stderr, "test failed input='%s' expect=%lu actual=%lu\n",
			input, expect, actual);
		abort();
	}
}


// ----------------------------------------------

static const char test_data[] = 
	"3,4,3,1,2";

int main() {
	test(test_data, part1, 5934);
	test(test_data, part2, 26984457539);

	struct Fish fish;
	read_input(file_reader("day6/input.txt"), &fish);

	printf("Part 1: %lu\n", part1(fish));
	printf("Part 2: %lu\n", part2(fish));

	return 0;
}