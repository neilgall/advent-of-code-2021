#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ----------------------------------------------
// Abstract reader

struct Reader;
typedef void (*reader_delete_f)(struct Reader *);
typedef char (*reader_next_f)(struct Reader *);

struct Reader {
	reader_next_f next;
	reader_delete_f delete;
};


// ----------------------------------------------
// String reader

struct StringReader {
	struct Reader reader;
	const char *p;
};

char string_reader_f(struct StringReader *sr) {
	return *(sr->p++);
}

struct Reader *string_reader(const char *s) {
	struct StringReader *sr = (struct StringReader *)malloc(sizeof(struct StringReader));
	sr->reader.next = (reader_next_f)&string_reader_f;
	sr->reader.delete = (reader_delete_f)&free;
	sr->p = s;
	return (struct Reader *)sr;
}


// ----------------------------------------------
// File reader

struct FileReader {
	struct Reader reader;
	FILE *fd;
};

char file_reader_f(struct FileReader *fr) {
	int c = fgetc(fr->fd);
	return (c == EOF) ? 0 : (char)c;
}

void file_reader_delete(struct FileReader *fr) {
	fclose(fr->fd);
	free(fr);
}

struct Reader *file_reader(const char *filename) {
	FILE *f = fopen(filename, "rt");
	if (f == NULL) {
		perror("can't open file");
		abort();
	}
	struct FileReader *fr = (struct FileReader *)malloc(sizeof(struct FileReader));
	fr->reader.next = (reader_next_f)&file_reader_f;
	fr->reader.delete = (reader_delete_f)&file_reader_delete;
	fr->fd = f;
	return (struct Reader *)fr;
}


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
			append_entry(&report, entry);
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


// ----------------------------------------------
// tests

void test(const char *input, int part1_expect) {
	struct Report *report = read_report(string_reader(input));
	int increments = part1(report);
	free(report);
	assert(increments == part1_expect);
}


// ----------------------------------------------

int main() {
	test("199\n200\n208\n210\n200\n207\n240\n269\n260\n263", 7);

	struct Report *report = read_report(file_reader("day1/input.txt"));

	printf("Part 1: %d\n", part1(report));

	free(report);
	return 0;
}