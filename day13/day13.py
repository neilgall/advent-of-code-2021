
def parse(line):
    if line.startswith("fold along"):
        fold = line[11:].split("=")
        return "fold", fold[0], int(fold[1])
    else:
        dot = [int(x) for x in line.split(",")]
        return "dot", dot[0], dot[1]


def fold(f, a):
    return a if a < f else 2*f-a


def apply_fold(dots, axis, coord):
    if axis == 'x':
        yield from ((fold(coord, x), y) for (x,y) in dots)
    else:
        yield from ((x, fold(coord, y)) for (x,y) in dots)
    

def part1(data):
    dots = [(x,y) for t, x, y in data if t == 'dot']
    fold = [(a, c) for t, a, c in data if t == 'fold'][0]
    folded = set(apply_fold(dots, *fold))
    return len(folded)


test_input = """
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"""

if __name__ == "__main__":
    test_data = [parse(line) 
        for line in test_input.split('\n')
        if line.strip() != ''
    ]
    assert test_data[0] == ('dot', 6, 10)
    assert test_data[-1] == ('fold', 'x', 5)
    assert part1(test_data) == 17

    input = [parse(line) 
        for line in open('input.txt').readlines()
        if line.strip() != ''
    ]
    print("Part 1: ", part1(input))
