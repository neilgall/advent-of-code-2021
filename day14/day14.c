#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"

struct instructions {
    size_t template_length;    
    char template[32];
    char rules[26*26];
};

struct instructions *parse(struct Reader *reader) {
    struct instructions *i = (struct instructions *)malloc(sizeof(struct instructions));
    memset(i, 0, sizeof(struct instructions));
    int state = 0;
    size_t rule_index = 0;
    char c;
    while ((c = reader->next(reader)) != 0) {
        switch (state) {
            case 0:
                if (isalpha(c)) {
                    i->template[i->template_length++] = c;
                } else {
                    state = 1;
                }
                break;
            case 1:
                if (isalpha(c)) {
                    rule_index = (c - 'A') * 26;
                    state = 2;
                }
                break;
            case 2:
                if (isalpha(c)) {
                    rule_index += (c - 'A');
                    state = 3;
                }
                break;
            case 3:
                if (isalpha(c)) {
                    i->rules[rule_index] = c;
                    state = 1;
                }
                break;
        }
    }
    assert(state == 1);
    free(reader);
    return i;
}

struct polymer {
    size_t capacity;
    size_t length;
    char elements[];
};

struct polymer *new_polymer(size_t capacity) {
    size_t bytes = sizeof(struct polymer) + capacity;
    struct polymer *p = (struct polymer *)malloc(bytes);
    memset(p, 0, bytes);
    p->capacity = capacity;
    return p;
}

struct polymer *polymer_from_template(struct instructions *i) {
    struct polymer *p = new_polymer(i->template_length);
    p->length = i->template_length;
    memcpy(p->elements, i->template, i->template_length);
    return p;
}

void append_polymer(struct polymer *p, char c) {
    assert(p->length < p->capacity);
    p->elements[p->length++] = c;
}

struct polymer *grow_polymer(struct polymer *p, struct instructions *i) {
    struct polymer *new = new_polymer(p->length * 2);
    for (size_t e = 0; e < p->length-1; ++e) {
        append_polymer(new, p->elements[e]);
        size_t rule_index = (p->elements[e] - 'A') * 26 + p->elements[e+1] - 'A';
        char insert = i->rules[rule_index];
        if (isalpha(insert)) {
            append_polymer(new, insert);
        }
    }
    append_polymer(new, p->elements[p->length-1]);
    return new;
}   

int compare_ints(const void *a, const void *b) {
    return *(int *)a - *(int *)b;
}

int part1(struct instructions *instr) {

    // run polymer growth 10 times
    struct polymer *p = polymer_from_template(instr);
    for (int i = 0; i < 10; ++i) {
        struct polymer *new = grow_polymer(p, instr);
        free(p);
        p = new;
    }

    // count frequencies of each element
    int freq[26] = { 0 };
    for (size_t i = 0; i < p->length; ++i) {
        freq[p->elements[i] - 'A']++;
    }
    qsort(freq, 26, sizeof(int), compare_ints);
    
    // find most and least frequent
    size_t min = 0;
    while (min < 26 && freq[min] == 0) min++;
    size_t max = 25;
    while (max > 0 && freq[max] == 0) max--;

    free(p);
    return freq[max] - freq[min];
}

static const char test_data[] =
    "NNCB\n\n"
    "CH -> B\n"
    "HH -> N\n"
    "CB -> H\n"
    "NH -> C\n"
    "HB -> C\n"
    "HC -> B\n"
    "HN -> C\n"
    "NN -> C\n"
    "BH -> H\n"
    "NC -> B\n"
    "NB -> B\n"
    "BN -> B\n"
    "BB -> N\n"
    "BC -> B\n"
    "CC -> N\n"
    "CN -> C\n";

void tests() {
    struct instructions *i = parse(string_reader(test_data));
    assert(i->template_length == 4);
    assert(strcmp(i->template, "NNCB") == 0);
    assert(i->rules[('C'-'A')*26 + 'H'-'A'] == 'B');
    assert(part1(i) == 1588);
    free(i);
}

int main() {
    tests();

    struct instructions *i = parse(file_reader("day14/input.txt"));
    printf("Part 1: %d\n", part1(i));

    free(i);
    return 0;
}  