// ----------------------------------------------
// Abstract reader

struct Reader;
typedef void (*reader_delete_f)(struct Reader *);
typedef char (*reader_next_f)(struct Reader *);

struct Reader {
	reader_next_f next;
	reader_delete_f delete;
};


struct Reader *string_reader(const char *s);
struct Reader *file_reader(const char *filename);
