#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bool.h"
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

typedef unsigned char pixel_t;

struct enhancement {
    pixel_t pixels[512];
};

struct image {
	size_t alloc_width;
	size_t alloc_height;
	size_t width;
	size_t height;
    pixel_t default_pixel;
	pixel_t pixels[];
};

struct input_data {
    struct enhancement *enhancement;
    struct image *image;
};

void free_input_data(struct input_data *input) {
    free(input->enhancement);
    free(input->image);
}

size_t image_bytes(size_t w, size_t h) {
	return sizeof(struct image) + sizeof(pixel_t) * w * h;
}

struct image *new_image(size_t w, size_t h) {
	struct image *img = (struct image *)malloc(image_bytes(w, h));
	img->alloc_width = w;
	img->alloc_height = h;
	img->width = w;
	img->height = h;
    img->default_pixel = 0;
	memset(img->pixels, 0, sizeof(pixel_t) * w * h);
	return img;
}

bool_t in_bounds(const struct image *img, struct point p) {
	return 0 <= p.x && 0 <= p.y && p.x < img->width && p.y < img->height;
}

pixel_t pixel_at(const struct image *img, struct point p) {
	return in_bounds(img, p)
        ? img->pixels[p.y * img->width + p.x]
        : img->default_pixel;
}

void set_pixel_at(struct image *img, struct point p, pixel_t pixel) {
	assert(in_bounds(img, p));
	img->pixels[p.y * img->width + p.x] = pixel;
}

enum resize {
	HORIZONTAL,
	VERTICAL
};

struct image *copy_image(const struct image *img) {
	size_t size = image_bytes(img->alloc_width, img->alloc_height);
	struct image *copy = (struct image *)malloc(size);
	memcpy(copy, img, size);
	return copy;
}

struct image *resize_image(struct image *old, enum resize resize) {
	struct image *new = resize == HORIZONTAL 
		? new_image(old->alloc_width * 2, old->alloc_height)
		: new_image(old->alloc_width, old->alloc_height * 2);
	new->width = old->width;
	new->height = old->height;
	for (size_t y = 0; y < old->height; ++y)
		for (size_t x = 0; x < old->width; ++x) {
			struct point p = { .x = x, .y = y };
			set_pixel_at(new, p, pixel_at(old, p));
		}
	free(old);
	return new;
}

struct enhancement *read_enhancement(struct Reader *reader) {
    struct enhancement *enh = (struct enhancement *)malloc(sizeof(struct enhancement));
    for (size_t i = 0; i < 512; ++i) {
        char c = reader->next(reader);
        assert(c == '#' || c == '.');
        enh->pixels[i] = (c == '#') ? 1 : 0;
    }
    return enh;
}

struct image *read_image(struct Reader *reader) {
	struct image *img = new_image(2, 2);
	struct point p = { .x = 0, .y = 0 };
	char c;
	while ((c = reader->next(reader)) != 0) {
		if (c == '#' || c == '.') {
			if (p.x == img->alloc_width)
				img = resize_image(img, HORIZONTAL);
			if (p.x == img->width)
				img->width++;
			set_pixel_at(img, p, c == '#' ? 1 : 0);
			p.x++;
		} else if (p.x > 0) {
			if (++p.y == img->alloc_height)
				img = resize_image(img, VERTICAL);
			if (p.y == img->height)
				img->height++;
			p.x = 0;
		}
	}
	img->height = (p.x == 0) ? p.y : p.y+1;

    printf("loaded image size=%d,%d\n", img->width, img->height);
	return img;
}

void skip_whitespace(struct Reader *reader) {
    char c;
    while (isspace(reader->peek(reader))) {
        reader->next(reader);
    }
}

void read_input(struct Reader *reader, struct input_data *input) {
    input->enhancement = read_enhancement(reader);
    skip_whitespace(reader);    
    input->image = read_image(reader);
    free(reader);
}


// debug

void print_image(const struct image *img) {
    for (size_t y = 0; y < img->height; ++y) {
        for (size_t x = 0; x < img->width; ++x) {
            struct point p = { .x = x, .y = y };
            putchar(pixel_at(img, p) ? '#' : '.');
        }
        putchar('\n');
    }
}

// problems

unsigned read_3x3(const struct image *img, const struct point p) {
    unsigned nine = 0;
    for (int y = p.y-1; y <= p.y+1; ++y) {
        for (int x = p.x-1; x <= p.x+1; ++x) {
            struct point q = { .x = x, .y = y };
            nine = (nine << 1) | pixel_at(img, q);
        }
    }
    return nine;
}

struct image *enhance(const struct image *img, const struct enhancement *enh) {
    struct image *new = new_image(img->width + 2, img->height + 2);
    for (int y = 0; y < new->height; ++y) {
        for (int x = 0; x < new->width; ++x) {
            struct point oldp = { .x = x-1, .y = y-1 };
            struct point newp = { .x = x,   .y = y   };
            unsigned nine = read_3x3(img, oldp);
            set_pixel_at(new, newp, enh->pixels[nine]);
        }
    }
    new->default_pixel = enh->pixels[img->default_pixel ? 511 : 0];
    return new;
}

size_t count_pixels(const struct image *img) {
    size_t count = 0;
    for (size_t y = 0; y < img->height; ++y) {
        for (size_t x = 0; x < img->width; ++x) {
            struct point p = { .x = x, .y = y };
            count += pixel_at(img, p);
        }
    }
    return count;
}

int part1(const struct input_data *input) {
    printf("original\n"); print_image(input->image);
    struct image *pass1 = enhance(input->image, input->enhancement);
    printf("pass 1\n"); print_image(pass1);
    struct image *pass2 = enhance(pass1, input->enhancement);
    printf("pass 2\n"); print_image(pass2);
    size_t pixels_lit = count_pixels(pass2);
    free(pass1);
    free(pass2);
    printf("pixels lit %lu\n", pixels_lit);
    return pixels_lit;
}


// tests

static const char test_data[] = 
    "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##"
    "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###"
    ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#."
    ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#....."
    ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.."
    "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#....."
    "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n"
    "\n"
    "#..#.\n"
    "#....\n"
    "##..#\n"
    "..#..\n"
    "..###";

void tests() {
    struct input_data input;
    read_input(string_reader(test_data), &input);
	assert(part1(&input) == 35);
    free_input_data(&input);
}

void tests2() {
    struct input_data input;
    read_input(file_reader("day20/example.txt"), &input);
	assert(part1(&input) == 5326);
    free_input_data(&input);
}

// main
int main() {
	tests();
    tests2();

    struct input_data input;
    read_input(file_reader("day20/input.txt"), &input);

	printf("Part 1: %lu\n", part1(&input));
	
    free_input_data(&input);
	return 0;
}