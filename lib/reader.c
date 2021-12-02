#include <stdio.h>
#include <stdlib.h>
#include "reader.h"


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
