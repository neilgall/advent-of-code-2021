#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"

// model

struct point {
	size_t x;
	size_t y;
};

struct point up(struct point p) {
	struct point up = { .x = p.x, .y = p.y-1 };
	return up;
}

struct point down(struct point p) {
	struct point down = { .x = p.x, .y = p.y+1 };
	return down;
}

struct point left(struct point p) {
	struct point left = { .x = p.x-1, .y = p.y };
	return left;
}

struct point right(struct point p) {
	struct point right = { .x = p.x+1, .y = p.y };
	return right;
}

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

bool_t in_map(const struct map *map, struct point p) {
	// printf("in map %u,%u\n", x, y);
	return 0 <= p.x && 0 <= p.y && p.x < map->width && p.y < map->height;
}

cell_t map_at(const struct map *map, struct point p) {
	assert(in_map(map, p));
	return map->cells[p.y * map->width + p.x];
}

void set_map_at(struct map *map, struct point p, cell_t cell) {
	assert(in_map(map, p));
	map->cells[p.y * map->width + p.x] = cell;
}

enum resize {
	HORIZONTAL,
	VERTICAL
};

struct map *copy_map(const struct map *map) {
	size_t size = map_bytes(map->alloc_width, map->alloc_height);
	struct map *copy = (struct map *)malloc(size);
	memcpy(copy, map, size);
	return copy;
}

struct map *resize_map(struct map *old, enum resize resize) {
	struct map *new = resize == HORIZONTAL 
		? new_map(old->alloc_width * 2, old->alloc_height)
		: new_map(old->alloc_width, old->alloc_height * 2);
	new->width = old->width;
	new->height = old->height;
	for (size_t y = 0; y < old->height; ++y)
		for (size_t x = 0; x < old->width; ++x) {
			struct point p = { .x = x, .y = y };
			set_map_at(new, p, map_at(old, p));
		}
	free(old);
	return new;
}

struct map *read_map(struct Reader *reader) {
	struct map *map = new_map(2, 2);
	struct point p = { .x = 0, .y = 0 };
	char c;
	while ((c = reader->next(reader)) != 0) {
		if (isdigit(c)) {
			if (p.x == map->alloc_width)
				map = resize_map(map, HORIZONTAL);
			if (p.x == map->width)
				map->width++;
			set_map_at(map, p, c - '0');
			p.x++;
		} else if (p.x > 0) {
			if (++p.y == map->alloc_height)
				map = resize_map(map, VERTICAL);
			if (p.y == map->height)
				map->height++;
			p.x = 0;
		}
	}
	map->height = (p.x == 0) ? p.y : p.y+1;

	printf("loaded map alloc=%u,%u size=%u,%u\n",
		map->alloc_width, map->alloc_height, map->width, map->height);

	reader->delete(reader);
	return map;
}

// part 1

typedef void (*handler)(const struct map *, struct point, void *);

void find_basins(const struct map *map, void *data, handler f) {
	for (size_t y = 0; y < map->height; ++y) {
		for (size_t x = 0; x < map->width; ++x) {
			struct point p = { .x = x, .y = y };
			cell_t height = map_at(map, p);
			if (in_map(map, up(p)) && map_at(map, up(p)) <= height)
				continue;
			if (in_map(map, down(p)) && map_at(map, down(p)) <= height)
				continue;
			if (in_map(map, left(p)) && map_at(map, left(p)) <= height)
				continue;
			if (in_map(map, right(p)) && map_at(map, right(p)) <= height)
				continue;
			f(map, p, data);
		}
	}
}

void part1_handler(const struct map *map, struct point p, void *data) {
	size_t *risk_level = (size_t *)data;
	*risk_level += 1 + map_at(map, p);
}

size_t part1(const struct map *map) {
	size_t risk_level = 0;
	find_basins(map, &risk_level, part1_handler);
	return risk_level;
}


// part 2

struct basins {
	size_t count;
	size_t capacity;
	size_t sizes[];
};

struct basins *new_basins() {
	size_t size = sizeof(struct basins) + sizeof(size_t);
	struct basins *basins = (struct basins *)malloc(size);
	basins->count = 0;
	basins->capacity = 1;
	return basins;
}

struct basins *add_basin(struct basins *basins, size_t basin_size) {
	if (basins->count == basins->capacity) {
		size_t new_capacity = basins->capacity * 2;
		size_t new_size = sizeof(struct basins) + sizeof(size_t) * new_capacity;
		basins = (struct basins *)realloc(basins, new_size);
		basins->capacity = new_capacity;
	}
	basins->sizes[basins->count++] = basin_size;
	return basins;
}

int compare_sizes(const void *a, const void *b) {
	int ia = (int) *(const size_t *)a;
	int ib = (int) *(const size_t *)b;
	return ib - ia;
}

void sort_basins(struct basins *basins) {
	qsort(basins->sizes, basins->count, sizeof(size_t), compare_sizes);
}

void find_basin_size(struct map *map, struct point p, size_t *basin_size) {
	if (!in_map(map, p) || map_at(map, p) == 9)
		return;
	set_map_at(map, p, 9);
	*basin_size += 1;
	find_basin_size(map, up(p), basin_size);
	find_basin_size(map, down(p), basin_size);
	find_basin_size(map, left(p), basin_size);
	find_basin_size(map, right(p), basin_size);
}

void part2_handler(const struct map *map, struct point p, void *data) {
	struct basins **basins_p = (struct basins **)data;

	struct map *copy = copy_map(map);
	size_t basin_size = 0;
	find_basin_size(copy, p, &basin_size);
	free(copy);

	*basins_p = add_basin(*basins_p, basin_size);
}

size_t part2(const struct map *map) {
	struct basins *basins = new_basins();
	find_basins(map, &basins, part2_handler);
	sort_basins(basins);
	assert(basins->count >= 3);
	size_t result = basins->sizes[0] * basins->sizes[1] * basins->sizes[2];
	free(basins);
	return result;
}

// tests

static const char test_data[] = 
	"2199943210\n3987894921\n9856789892\n8767896789\n9899965678";

void tests() {
	struct map *map = read_map(string_reader(test_data));
	assert(part1(map) == 15);
	assert(part2(map) == 1134);
	free(map);
}

// main
int main() {
	tests();

	struct map *map = read_map(file_reader("day9/input.txt"));
	printf("Part 1: %lu\n", part1(map));
	printf("Part 2: %lu\n", part2(map));
	free(map);

	return 0;
}