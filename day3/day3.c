#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "reader.h"

// ----------------------------------------------
// Report structure

typedef uint16_t entry_t;

struct Report {
	size_t capacity;
	size_t count;
	size_t entry_length;
	entry_t entries[];
};

size_t report_size(size_t count) {
	return sizeof(struct Report) + sizeof(entry_t) * count;
}

struct Report *empty_report(size_t capacity) {
	struct Report *report = (struct Report *)malloc(report_size(capacity));
	report->capacity = capacity;
	report->count = 0;
	report->entry_length = 0;
	return report;
}

void append_entry(struct Report **report_p, entry_t entry, size_t length) {
	struct Report *report = *report_p;
	if (report->count == report->capacity) {
		report->capacity *= 2;
		*report_p = report = (struct Report *)realloc(report, report_size(report->capacity));
	}
	if (length > report->entry_length)
		report->entry_length = length;
	report->entries[report->count++] = entry;
}

struct Report *read_report(struct Reader *reader) {
	struct Report *report = empty_report(100);
	entry_t entry = 0;
	size_t length = 0;
	char c;
	while ((c = reader->next(reader)) != 0) {
		if (isdigit(c)) {
			entry = (entry << 1) + (c - '0');
			length += 1;
		} else {
			if (length > 0) {
				append_entry(&report, entry, length);
			}
			entry = 0;
			length = 0;
		}
	}
	if (length > 0) {
		append_entry(&report, entry, length);
	}
	reader->delete(reader);
	return report;
}


// ----------------------------------------------
// Part 1

int part1(struct Report *report) {
	unsigned gamma = 0;
	unsigned epsilon = 0;
	for (size_t i = 0; i < report->entry_length; ++i) {
		entry_t bit = 1 << i;
		size_t ones = 0;
		for (size_t e = 0; e < report->count; ++e) {
			if (report->entries[e] & bit) ones++;
		}
		if (ones * 2 > report->count) {
			gamma |= bit;
		} else {
			epsilon |= bit;
		}
	}
	return gamma * epsilon;
}


// ----------------------------------------------
// tests

void test(const char *input, int (*f)(struct Report *), int expect) {
	struct Report *report = read_report(string_reader(input));
	int actual = f(report);
	free(report);
	if (actual != expect) {
		fprintf(stderr, "test failed input='%s' expect=%d actual=%d\n",
			input, expect, actual);
		abort();
	}
}

static const char test_data[] = 
	"00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010";

void tests() {
	test(test_data, part1, 198);
}

// ----------------------------------------------
// Main

int main() {
	tests();

	struct Report *report = read_report(file_reader("day3/input.txt"));

	printf("Part 1: %d\n", part1(report));

	return 0;
}