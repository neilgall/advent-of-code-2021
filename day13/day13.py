
def parse(line):
    if line.startswith("fold along"):
        fold = line[11:].split("=")
        return "fold", fold[0], int(fold[1])
    else:
        dot = [int(x) for x in line.split(",")]
        return "dot", dot[0], dot[1]


def parse_data(input):
    data = [parse(line) for line in input.split('\n') if line.strip() != '']
    dots = [(x,y) for t, x, y in data if t == 'dot']
    folds = [(a, c) for t, a, c in data if t == 'fold']
    return dots, folds


def fold(f, a):
    return a if a < f else 2*f-a


def apply_fold(dots, axis, coord):
    if axis == 'x':
        yield from ((fold(coord, x), y) for (x,y) in dots)
    else:
        yield from ((x, fold(coord, y)) for (x,y) in dots)
    

def part1(dots, folds):
    folded = set(apply_fold(dots, *folds[0]))
    return len(folded)


def part2(dots, folds):
    for fold in folds:
        dots = set(apply_fold(dots, *fold))
    return dots

def draw(dots):
    minx = min(x for x,_ in dots)
    maxx = max(x for x,_ in dots)
    miny = min(y for _,y in dots)
    maxy = max(y for _,y in dots)
    for y in range(miny, maxy+1):
        line = ''
        for x in range(minx, maxx+1):
            line += '#' if (x, y) in dots else '.'
        yield line


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

def tests():
    dots, folds = parse_data(test_input)
    assert dots[0] == ('dot', 6, 10)
    assert folds[-1] == ('fold', 'x', 5)
    assert part1(dots, folds) == 17
    image = list(draw(part2(dots, folds)))
    assert image == ['#####','#...#','#...#','#...#','#####','.....','.....']


if __name__ == "__main__":
    dots, folds = parse_data(open('input.txt').read())
    print("Part 1: ", part1(dots, folds))
    
    for line in draw(part2(dots, folds)):
        print(line)
    
