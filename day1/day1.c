
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include "reader.h"

// ----------------------------------------------
// Report structure

struct Report {
	size_t capacity;
	size_t count;
	int entries[];
};

size_t report_size(size_t count) {
	return sizeof(struct Report) + sizeof(int) * count;
}

struct Report *empty_report(size_t capacity) {
	struct Report *report = (struct Report *)malloc(report_size(capacity));
	report->capacity = capacity;
	report->count = 0;
	return report;
}

void append_entry(struct Report **report_p, int entry) {
	struct Report *report = *report_p;
	if (report->count == report->capacity) {
		report->capacity *= 2;
		*report_p = report = (struct Report *)realloc(report, report_size(report->capacity));
	}
	report->entries[report->count++] = entry;

}

struct Report *read_report(struct Reader *reader) {
	struct Report *report = empty_report(100);
	int entry = 0;
	int entry_digits = 0;
	char c;
	while ((c = reader->next(reader)) != 0) {
		if (isdigit(c)) {
			entry = (entry * 10) + (c - '0');
			entry_digits += 1;
		} else {
			if (entry_digits > 0) {
				append_entry(&report, entry);
			}
			entry = 0;
			entry_digits = 0;
		}
	}
	if (entry_digits > 0) {
		append_entry(&report, entry);
	}
	reader->delete(reader);
	return report;
}


// ----------------------------------------------
// Problems

int part1(struct Report *report) {
	size_t increments = 0;
	for (size_t pos = 1; pos < report->count; ++pos) {
		if (report->entries[pos] > report->entries[pos-1])
			increments++;
	}
	return increments;
}


int window(struct Report *report, int pos) {
	int *es = report->entries + pos;
	return es[0] + es[1] + es[2];
}

int part2(struct Report *report) {
	size_t increments = 0;
	for (size_t pos = 3; pos < report->count; ++pos) {
		int prev = window(report, pos-3);
		int curr = window(report, pos-2);
		if (curr > prev) 
			increments++;
	}
	return increments;
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


// ----------------------------------------------

static const char test_data[] = 
	"199\n200 208  210 200 207   240 269 260\n263";

int main() {
	test(test_data, part1, 7);
	test(test_data, part2, 5);

	struct Report *report = read_report(file_reader("day1/input.txt"));

	printf("Part 1: %d\n", part1(report));
	printf("Part 2: %d\n", part2(report));

	free(report);
	return 0;
}