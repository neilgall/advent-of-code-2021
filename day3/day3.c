#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "reader.h"
#include "measure.h"

// ----------------------------------------------
// Report structure

typedef struct entry {
	uint16_t value;
	uint8_t keep;
} entry_t;

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
	struct entry entry = { .value = 0, .keep = 1 };
	size_t length = 0;
	char c;
	while ((c = reader->next(reader)) != 0) {
		if (isdigit(c)) {
			entry.value = (entry.value << 1) + (c - '0');
			length += 1;
		} else {
			if (length > 0) {
				append_entry(&report, entry, length);
			}
			entry.value = 0;
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
// Problems

size_t ones_in_bit_position(struct Report *report, unsigned bit) {
	size_t ones = 0;
	for (size_t e = 0; e < report->count; ++e) {
		if (report->entries[e].keep && report->entries[e].value & bit)
			ones++;
	}
	return ones;	
}


int part1(struct Report *report) {
	unsigned gamma = 0;
	unsigned epsilon = 0;
	for (size_t i = 0; i < report->entry_length; ++i) {
		unsigned bit = 1 << i;
		size_t ones = ones_in_bit_position(report, bit);
		if (ones * 2 > report->count) {
			gamma |= bit;
		} else {
			epsilon |= bit;
		}
	}
	return gamma * epsilon;
}


void keep_all_entries(struct Report *report) {
	for (size_t i = 0; i < report->count; ++i) {
		report->entries[i].keep = 1;
	}
}


size_t discard_entries(struct Report *report, unsigned bit, unsigned discard) {
	size_t discarded = 0;
	for (size_t i = 0; i < report->count; ++i) {
		if (report->entries[i].keep &&
				!(report->entries[i].value & bit) == discard) {
			report->entries[i].keep = 0;
			discarded++;
		}
	}
	return discarded;
}


unsigned first_kept_entry(struct Report *report) {
	for (size_t i = 0; i < report->count; ++i) {
		if (report->entries[i].keep)
			return report->entries[i].value;
	}
	abort();
}


unsigned oxygen_generator_rating(struct Report *report) {
	size_t remaining = report->count;
	size_t bit_pos = report->entry_length;

	keep_all_entries(report);
	while (bit_pos-- > 0 && remaining > 1) {
		unsigned bit = 1 << bit_pos;
		size_t ones = ones_in_bit_position(report, bit);
		unsigned keep = (ones * 2 >= remaining) ? 1 : 0;
		remaining -= discard_entries(report, bit, !keep);
	}

	return first_kept_entry(report);
}


unsigned co2_scrubber_rating(struct Report *report) {
	size_t remaining = report->count;
	size_t bit_pos = report->entry_length;

	keep_all_entries(report);
	while (bit_pos-- > 0 && remaining > 1) {
		unsigned bit = 1 << bit_pos;
		size_t ones = ones_in_bit_position(report, bit);
		unsigned keep = (ones * 2 < remaining) ? 1 : 0;
		remaining -= discard_entries(report, bit, !keep);
	}

	return first_kept_entry(report);
}


int part2(struct Report *report) {
	return oxygen_generator_rating(report) * co2_scrubber_rating(report);
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
	test(test_data, part2, 230);
}

void run(struct Report *report) {
	part1(report);
	part2(report);
}

// ----------------------------------------------
// Main

int main() {
	tests();

	struct Report *report = read_report(file_reader("day3/input.txt"));

	printf("Part 1: %d\n", part1(report));
	printf("Part 2: %d\n", part2(report));
	printf("Execution time: %.6f\n", measure(1000, (measure_f)run, report));

	free(report);
	return 0;
}