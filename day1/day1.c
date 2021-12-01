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