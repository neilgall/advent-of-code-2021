#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"

struct instructions {
    size_t template_length;    
    char template[32];
    char rules[26][26];
};

struct instructions *parse(struct Reader *reader) {
    struct instructions *i = (struct instructions *)malloc(sizeof(struct instructions));
    memset(i, 0, sizeof(struct instructions));
    int state = 0;
    int rule_left = 0;
    int rule_right = 0;
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
                    rule_left = c - 'A';
                    state = 2;
                }
                break;
            case 2:
                if (isalpha(c)) {
                    rule_right = c - 'A';
                    state = 3;
                }
                break;
            case 3:
                if (isalpha(c)) {
                    i->rules[rule_left][rule_right] = c;
                    state = 1;
                }
                break;
        }
    }
    assert(state == 1);
    free(reader);
    return i;
}

void template_pair(struct instructions *i, size_t index, int *left, int *right) {
    *left = i->template[index] - 'A';
    *right = i->template[index+1] - 'A';
}

typedef long count_t;

struct polymer {
    count_t counts[26][26];
};

void polymer_from_template(struct instructions *i, struct polymer *p) {
    memset(p, 0, sizeof(struct polymer));

    for (size_t e = 0; e < i->template_length-1; ++e) {
        int left, right;
        template_pair(i, e, &left, &right);
        p->counts[left][right]++;
    }
}

void print_pair_counts(struct polymer *p) {
    for (int a = 0; a < 26; ++a)
        for (int b = 0; b < 26; ++b)
            if (p->counts[a][b])
                printf("%c%c: %ld\n", a+'A', b+'A', p->counts[a][b]);
}


void grow_polymer(struct instructions *i, struct polymer *p) {
    count_t new[26][26] = { 0 };

    for (size_t left = 0; left < 26; ++left) {
        for (size_t right = 0; right < 26; ++right) {
            char insert = i->rules[left][right];
            count_t pair_count = p->counts[left][right];
            if (isalpha(insert) && pair_count > 0) {
                // printf("insert %c between %c,%c: %ld\n", insert, left+'A', right+'A', pair_count);
                insert -= 'A';
                new[left][insert] += pair_count;
                new[insert][right] += pair_count;
            } else if (pair_count > 0) {
                // printf("%c,%c unchanged %ld\n", left+'A', right+'A', pair_count);
                new[left][right] += pair_count;
            }
        }
    }

    memcpy(&p->counts, &new, sizeof(new));
    // print_pair_counts(p);
}   

count_t run_polymer_growth(struct instructions *instr, int steps) {

    // run polymer growth 10 times
    struct polymer p;
    polymer_from_template(instr, &p);

    for (int i = 0; i < steps; ++i) {
        grow_polymer(instr, &p);
    }

    // count frequencies of each element from the first of each pair
    count_t freq[26] = { 0 };
    for (int a = 0; a < 26; ++a) {
        for (int b = 0; b < 26; ++b) {
            freq[a] += p.counts[a][b];
        }
    }
    freq[instr->template[instr->template_length-1] - 'A'] += 1; // last element

    // find most and least frequent
    count_t min = LONG_MAX;
    count_t max = freq[0];
    for (int a = 1; a < 26; ++a) {
        if (freq[a] != 0 && freq[a] < min)
            min = freq[a];
        if (freq[a] > max)
            max = freq[a];
    }

    return max - min;
}

count_t part1(struct instructions *i) {
    return run_polymer_growth(i, 10);
}

count_t part2(struct instructions *i) {
    return run_polymer_growth(i, 40);
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
    assert(i->rules['C'-'A']['H'-'A'] == 'B');
    assert(part1(i) == 1588);
    assert(part2(i) == 2188189693529);
    free(i);
}

int main() {
    tests();

    struct instructions *i = parse(file_reader("day14/input.txt"));
    printf("Part 1: %ld\n", part1(i));
    printf("Part 2: %ld\n", part2(i));

    free(i);
    return 0;
}  