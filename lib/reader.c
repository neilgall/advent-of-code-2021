#include <stdio.h>
#include <stdlib.h>
#include "reader.h"


// ----------------------------------------------
// String reader

struct StringReader {
	struct Reader reader;
	const char *p;
};

char string_reader_next_f(struct StringReader *sr) {
	return *(sr->p++);
}

char string_reader_peek_f(struct StringReader *sr) {
	return *(sr->p);
}

struct Reader *string_reader(const char *s) {
	struct StringReader *sr = (struct StringReader *)malloc(sizeof(struct StringReader));
	sr->reader.next = (reader_next_f)&string_reader_next_f;
	sr->reader.peek = (reader_next_f)&string_reader_peek_f;
	sr->reader.delete = (reader_delete_f)&free;
	sr->p = s;
	return (struct Reader *)sr;
}


// ----------------------------------------------
// File reader

struct FileReader {
	struct Reader reader;
	FILE *fd;
	int peek;
};

void file_reader_fill_next(struct FileReader *fr) {
	if (feof(fr->fd)) {
		fr->peek = 0;
	} else {
		int c = fgetc(fr->fd);
		fr->peek = (c == EOF) ? 0 : c;
	}
}

char file_reader_next_f(struct FileReader *fr) {
	char c = fr->peek;
	file_reader_fill_next(fr);
	return c;
}

char file_reader_peek_f(struct FileReader *fr) {
	return fr->peek;
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
	fr->reader.next = (reader_next_f)&file_reader_next_f;
	fr->reader.peek = (reader_next_f)&file_reader_peek_f;
	fr->reader.delete = (reader_delete_f)&file_reader_delete;
	fr->fd = f;
	file_reader_fill_next(fr);
	return (struct Reader *)fr;
}
