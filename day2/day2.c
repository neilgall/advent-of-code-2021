#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include "reader.h"

// ----------------------------------------------
// Data model

enum Direction {
	FORWARD,
	DOWN,
	UP
};

struct Instruction {
	enum Direction direction;
	int amount;
};

struct Course {
	struct Instruction instruction;
	struct Course *next;
};

struct Course **add_instruction(struct Course **next, enum Direction direction, int amount) {
	struct Course *step = (struct Course *)malloc(sizeof(struct Course));
	step->instruction.direction = direction;
	step->instruction.amount = amount;
	step->next = NULL;
	*next = step;
	return &step->next;
}

struct Course *parse_course(struct Reader *reader) {
	struct Course *course = NULL;
	struct Course **next = &course;
	enum Direction direction;
	int amount;
	int state = 0;

	char c;
	while ((c = reader->next(reader)) != 0) {
		switch (state) {
			case 0:
				switch (c) {
					case 'f': 
						direction = FORWARD;
						state = 1;
						break;
					case 'd':
						direction = DOWN;
						state = 1;
						break;
					case 'u':
						direction = UP;
						state = 1;
						break;
				}
				break;
			case 1:
				if (isdigit(c)) {
					amount = c - '0';
					state = 2;
				}
				break;
			case 2:
				if (isdigit(c)) {
					amount = (amount * 10) + (c - '0');
				} else {
					next = add_instruction(next, direction, amount);
					state = 0;
				}
				break;
		}
	}
	if (state == 2) {
		add_instruction(next, direction, amount);
	}

	reader->delete(reader);

	return course;
}

void delete_course(struct Course *course) {
	while (course) {
		struct Course *next = course->next;
		free(course);
		course = next;
	}
}


// ----------------------------------------------
// Part 1

struct Position {
	int horizontal;
	int depth;
};

struct Position follow_course(struct Course *course) {
	struct Position pos = { .horizontal = 0, .depth = 0 };
	for (; course; course = course->next) {
		switch (course->instruction.direction) {
			case FORWARD:
				pos.horizontal += course->instruction.amount;
				break;
			case DOWN:
				pos.depth += course->instruction.amount;
				break;
			case UP:
				pos.depth -= course->instruction.amount;
				break;
		}
	}
	return pos;
}

int part1(struct Course *course) {
	struct Position pos = follow_course(course);
	return pos.horizontal * pos.depth;
}


// ----------------------------------------------
// Part 2

struct Position2 {
	int horizontal;
	int depth;
	int aim;
};

struct Position2 follow_course2(struct Course *course) {
	struct Position2 pos = { .horizontal = 0, .depth = 0, .aim = 0 };
	for (; course; course = course->next) {
		switch (course->instruction.direction) {
			case FORWARD:
				pos.horizontal += course->instruction.amount;
				pos.depth += pos.aim * course->instruction.amount;
				break;
			case DOWN:
				pos.aim += course->instruction.amount;
				break;
			case UP:
				pos.aim -= course->instruction.amount;
				break;
		}
	}
	return pos;
}

int part2(struct Course *course) {
	struct Position2 pos = follow_course2(course);
	return pos.horizontal * pos.depth;
}


// ----------------------------------------------
// tests

void test(const char *input, int (*f)(struct Course *), int expect) {
	struct Course *course = parse_course(string_reader(input));
	int actual = f(course);
	delete_course(course);
	if (actual != expect) {
		fprintf(stderr, "test failed input='%s' expect=%d actual=%d\n",
			input, expect, actual);
		abort();
	}
}

static const char test_data[] = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2";

int main() {
	test(test_data, part1, 150);
	test(test_data, part2, 900);

	struct Course *course = parse_course(file_reader("day2/input.txt"));

	printf("Part 1: %d\n", part1(course));
	printf("Part 2: %d\n", part2(course));

	delete_course(course);
	return 0;

}