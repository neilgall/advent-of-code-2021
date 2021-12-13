#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"

// model

struct point {
	int x;
	int y;
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
	size_t width;
	size_t height;
	cell_t cells[];
};

size_t map_bytes(size_t w, size_t h) {
	return sizeof(struct map) + sizeof(cell_t) * w * h;
}

struct map *new_map(size_t w, size_t h) {
	struct map *map = (struct map *)malloc(map_bytes(w, h));
	map->width = w;
	map->height = h;
	memset(map->cells, 0, sizeof(cell_t) * w * h);
	return map;
}

bool_t in_map(const struct map *map, struct point p) {
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

struct map *copy_map(const struct map *map) {
	size_t size = map_bytes(map->width, map->height);
	struct map *copy = (struct map *)malloc(size);
	memcpy(copy, map, size);
	return copy;
}

void print_map(const struct map *map) {
	for (int y = 0; y < map->height; ++y) {
		for (int x = 0; x < map->width; ++x) {
			struct point p = { .x = x, .y = y };
			cell_t energy = map_at(map, p);
			if (energy > 9)
				printf("*");
			else
				printf("%1d", energy);
		}
		printf("\n");
	}
}

struct map *read_map(struct Reader *reader) {
	struct map *map = new_map(10, 10);
	struct point p = { .x = 0, .y = 0 };
	char c;
	while ((c = reader->next(reader)) != 0) {
		if (isdigit(c)) {
			assert(p.x < map->width);
			set_map_at(map, p, c - '0');
			p.x++;
		} else if (p.x > 0) {
			assert(p.y < map->height);
			p.y++;
			p.x = 0;
		}
	}

	reader->delete(reader);
	return map;
}

// part 1

void increment_energy(struct map *map) {
	for (size_t y = 0; y < map->height; ++y) {
		for (size_t x = 0; x < map->width; ++x) {
			struct point p = { .x = x, .y = y };
			set_map_at(map, p, map_at(map, p) + 1);
		}
	}
}

int increment_neighbours(struct map *map, struct point p) {
	int count = 0;
	// printf("increment neighbours of %d,%d\n", p.x, p.y);
	for (int ny = p.y-1; ny <= p.y+1; ++ny) {
		for (int nx = p.x-1; nx <= p.x+1; ++nx) {
			struct point n = { .x = nx, .y = ny };
			if (in_map(map, n) && (n.y != p.y || n.x != p.x)) {
				cell_t energy = map_at(map, n) + 1;
				// printf("neighbour %d,%d energy %d\n", n.x, n.y, energy);
				set_map_at(map, n, energy);
				if (energy > 9 && energy < 100) {
					count++;
				}
			}
		}
	}
	return count;
}

int flash_octopii(struct map *map) {
	int count = 0;
	for (int y = 0; y < map->height; ++y) {
		for (int x = 0; x < map->width; ++x) {
			struct point p = { .x = x, .y = y };
			cell_t energy = map_at(map, p);
			if (energy > 9 && energy < 100) {
				set_map_at(map, p, 100);
				count += increment_neighbours(map, p);
			}
		}
	}
	return count;
}

int reset_octopii(struct map *map) {
	int flashes = 0;
	for (int y = 0; y < map->height; ++y) {
		for (int x = 0; x < map->width; ++x) {
			struct point p = { .x = x, .y = y };
			cell_t energy = map_at(map, p);
			if (energy > 9) {
				set_map_at(map, p, 0);
				flashes++;
			}
		}
	}	
	return flashes;
}

void swap_maps(struct map **map1, struct map **map2) {
	struct map *tmp = *map1;
	*map1 = *map2;
	*map2 = tmp;
}

int step_map(struct map *map) {
	increment_energy(map);
	while (flash_octopii(map) > 0) {}
	return reset_octopii(map);
}

int part1(const struct map *map) {
	struct map *copy = copy_map(map);
	int flashes = 0;
	for (int i = 0; i < 100; ++i) {
		flashes += step_map(copy);
	}
	free(copy);
	return flashes;
}

int part2(const struct map *map) {
	struct map *copy = copy_map(map);
	int steps = 1;
	while (step_map(copy) != 100) {
		steps++;
	}
	free(copy);
	return steps;
}

// tests

void tests() {
	struct map *map = read_map(file_reader("day11/example.txt"));
	assert(part1(map) == 1656);
	assert(part2(map) == 195);
	free(map);
}

// main
int main() {
	tests();

	struct map *map = read_map(file_reader("day11/input.txt"));
	printf("Part 1: %lu\n", part1(map));
	printf("Part 2: %lu\n", part2(map));
	free(map);

	return 0;
}