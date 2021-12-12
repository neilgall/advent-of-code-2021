#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"

// model

typedef unsigned char cell_t;
typedef unsigned char bool_t;

struct map {
	size_t alloc_width;
	size_t alloc_height;
	size_t width;
	size_t height;
	cell_t cells[];
};

size_t map_bytes(size_t w, size_t h) {
	return sizeof(struct map) + sizeof(cell_t) * w * h;
}

struct map *new_map(size_t w, size_t h) {
	struct map *map = (struct map *)malloc(map_bytes(w, h));
	map->alloc_width = w;
	map->alloc_height = h;
	map->width = w;
	map->height = h;
	memset(map->cells, 0, sizeof(cell_t) * w * h);
	return map;
}

bool_t in_map(const struct map *map, size_t x, size_t y) {
	// printf("in map %u,%u\n", x, y);
	return 0 <= x && 0 <= y && x < map->width && y < map->height;
}

cell_t map_at(const struct map *map, size_t x, size_t y) {
	assert(in_map(map, x, y));
	return map->cells[y * map->width + x];
}

void set_map_at(struct map *map, size_t x, size_t y, cell_t cell) {
	assert(in_map(map, x, y));
	map->cells[y * map->width + x] = cell;
}

enum resize {
	HORIZONTAL,
	VERTICAL
};

struct map *resize_map(struct map *old, enum resize resize) {
	struct map *new = resize == HORIZONTAL 
		? new_map(old->alloc_width * 2, old->alloc_height)
		: new_map(old->alloc_width, old->alloc_height * 2);
	new->width = old->width;
	new->height = old->height;
	for (size_t y = 0; y < old->height; ++y)
		for (size_t x = 0; x < old->width; ++x)
			set_map_at(new, x, y, map_at(old, x, y));
	free(old);
	return new;
}

struct map *read_map(struct Reader *reader) {
	struct map *map = new_map(2, 2);
	size_t x = 0;
	size_t y = 0;
	char c;
	while ((c = reader->next(reader)) != 0) {
		if (isdigit(c)) {
			if (x == map->alloc_width)
				map = resize_map(map, HORIZONTAL);
			if (x == map->width)
				map->width++;
			set_map_at(map, x, y, c - '0');
			x++;
		} else if (x > 0) {
			if (++y == map->alloc_height)
				map = resize_map(map, VERTICAL);
			if (y == map->height)
				map->height++;
			x = 0;
		}
	}
	map->height = (x == 0) ? y : y+1;

	printf("loaded map alloc=%u,%u size=%u,%u\n",
		map->alloc_width, map->alloc_height, map->width, map->height);

	reader->delete(reader);
	return map;
}

// part 1

size_t part1(const struct map *map) {
	size_t risk_level = 0;
	for (size_t y = 0; y < map->height; ++y) {
		for (size_t x = 0; x < map->width; ++x) {
			cell_t height = map_at(map, x, y);
			if (in_map(map, x, y-1) && map_at(map, x, y-1) <= height)
				continue;
			if (in_map(map, x, y+1) && map_at(map, x, y+1) <= height)
				continue;
			if (in_map(map, x-1, y) && map_at(map, x-1, y) <= height)
				continue;
			if (in_map(map, x+1, y) && map_at(map, x+1, y) <= height)
				continue;
			risk_level += 1 + height;
		}
	}
	return risk_level;
}

// tests

static const char test_data[] = 
	"2199943210\n3987894921\n9856789892\n8767896789\n9899965678";

void tests() {
	struct map *map = read_map(string_reader(test_data));
	assert(part1(map) == 15);
	free(map);
}

// main
int main() {
	tests();

	struct map *map = read_map(file_reader("day9/input.txt"));
	printf("Part 1: %lu\n", part1(map));
	free(map);

	return 0;
}