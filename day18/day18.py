import pytest
from collections import namedtuple

pair = namedtuple('pair', ['value', 'depth'])


def flatten(snailfish):        
    def walk(value, depth = 0):
        if type(value) is int:
            yield pair(value, depth)
        else:
            yield from walk(value[0], depth+1)
            yield from walk(value[1], depth+1)

    return list(walk(snailfish))


def unflatten(pairs):
    class Stack:
        def __init__(self):
            self._stack = []

        def depth(self):
            return len(self._stack)

        def push_to(self, depth):
            while depth > self.depth():
                self._stack.append([])

        def append(self, x):
            self._stack[-1].append(x)
            assert len(self._stack[-1]) <= 2
            if self.depth() > 1 and len(self._stack[-1]) == 2:
                self.pop_to(self.depth()-1)

        def pop_to(self, depth):
            while depth < self.depth():
                pop = self._stack[-1]
                del self._stack[-1]
                self.append(pop)
            return self._stack[-1]

    stack = Stack()
    for p in pairs:
        p = pair._make(p)
        stack.push_to(p.depth)
        stack.pop_to(p.depth)
        stack.append(p.value)

    return stack.pop_to(1)


def reduce(pairs):
    def add_to_pair(p, value):
        return pair(p.value + value, p.depth)

    def halve(value):
        return value // 2, value // 2 + value % 2

    def explode(pairs, i, depth):
        assert pairs[i+1].depth == depth
        if i > 0:
            pairs[i-1] = add_to_pair(pairs[i-1], pairs[i].value)
        if i < len(pairs)-2:
            pairs[i+2] = add_to_pair(pairs[i+2], pairs[i+1].value)
        pairs[i+1] = pair(0, depth-1)
        del pairs[i]

    def split(pairs, i, value, depth):
        round_down, round_up = halve(value)
        pairs[i] = pair(round_down, depth+1)
        pairs.insert(i+1, pair(round_up, depth+1))

    def step(pairs):
        for i, (value, depth) in enumerate(pairs):
            if depth == 5:
                explode(pairs, i, depth)
                return True

        for i, (value, depth) in enumerate(pairs):
            if value >= 10:
                split(pairs, i, value, depth)
                return True

        return False

    output = pairs[:]
    while step(output):
        pass
    return output


def add(lhs, rhs):
    def inc_depth(p):
        return pair(p.value, p.depth+1)
    return reduce([inc_depth(p) for p in lhs] + [inc_depth(p) for p in rhs])


def sum_list(numbers):
    n = None
    for m in numbers:
        n = m if n is None else add(n, m)
    return n


def magnitude(snailfish):
    if type(snailfish) is int:
        return snailfish
    else:
        return 3*magnitude(snailfish[0]) + 2*magnitude(snailfish[1])


def part1(snailfish_list):
    sum = sum_list(flatten(n) for n in snailfish_list)
    return magnitude(unflatten(sum))


def part2(snailfish_list):
    largest = 0
    ns = [flatten(n) for n in snailfish_list]
    for i, n in enumerate(ns):
        for m in ns[i+1:]:
            largest = max(
                largest,
                magnitude(unflatten(add(n, m))),
                magnitude(unflatten(add(m, n)))
            )
    return largest


@pytest.mark.parametrize('input,output', [
    ([1,2], [(1,1), (2,1)]),
    ([1,[2,[3,[4,5]]]], [(1,1), (2,2), (3,3), (4,4), (5,4)]),
    ([[[[1,2],3],4],5], [(1,4), (2,4), (3,3), (4,2), (5,1)]),
    ([[3,[2,[8,0]]],[9,[5,[7,0]]]], [(3,2), (2,3), (8,4), (0,4), (9,2), (5,3), (7,4), (0,4)])
])
def test_flatten(input, output):
    assert flatten(input) == output


@pytest.mark.parametrize('pairs,snailfish', [
    ([(1,1), (2,1)], [1,2]),
    ([(1,1), (2,2), (3,3), (4,4), (5,4)], [1,[2,[3,[4,5]]]]),
    ([(1,4), (2,4), (3,3), (4,2), (5,1)], [[[[1,2],3],4],5]),
    ([(3,2), (2,3), (8,4), (0,4), (9,2), (5,3), (7,4), (0,4)], [[3,[2,[8,0]]],[9,[5,[7,0]]]])
])
def test_unflatten(pairs, snailfish):
    assert unflatten(pairs) == snailfish


@pytest.mark.parametrize('input,output', [
    ([[[[[9,8],1],2],3],4], [[[[0,9],2],3],4]),
    ([7,[6,[5,[4,[3,2]]]]], [7,[6,[5,[7,0]]]]),
    ([[6,[5,[4,[3,2]]]],1], [[6,[5,[7,0]]],3]),
    ([[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]], [[3,[2,[8,0]]],[9,[5,[7,0]]]]),
    ([[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]], [[3,[2,[8,0]]],[9,[5,[7,0]]]])
])
def test_reduce(input, output):
    assert unflatten(reduce(flatten(input))) == output


@pytest.mark.parametrize('lhs,rhs,result', [
    ([1,2], [[3,4],5], [[1,2],[[3,4],5]]),
    ([[[[4,3],4],4],[7,[[8,4],9]]], [1,1], [[[[0,7],4],[[7,8],[6,0]]],[8,1]])
])
def test_add(lhs, rhs, result):
    assert unflatten(add(flatten(lhs), flatten(rhs))) == result


@pytest.mark.parametrize('numbers,result', [
    (
        [
            [1,1],
            [2,2],
            [3,3],
            [4,4]
        ],
        [[[[1,1],[2,2]],[3,3]],[4,4]]
    ),
    (
        [
            [1,1],
            [2,2],
            [3,3],
            [4,4],[5,5]
        ],
        [[[[3,0],[5,3]],[4,4]],[5,5]]
    ),
    (
        [
            [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],
            [7,[[[3,7],[4,3]],[[6,3],[8,8]]]],
            [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]],
            [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]],
            [7,[5,[[3,8],[1,4]]]],
            [[2,[2,2]],[8,[8,1]]],
            [2,9],
            [1,[[[9,3],9],[[9,0],[0,7]]]],
            [[[5,[7,4]],7],1],
            [[[[4,2],2],6],[8,7]]
        ],
        [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
    )
])
def test_sum_list(numbers, result):
    ns = (flatten(n) for n in numbers)
    assert unflatten(sum_list(ns)) == result


@pytest.mark.parametrize('snailfish,expect', [
    ([[1,2],[[3,4],5]], 143),
    ([[[[0,7],4],[[7,8],[6,0]]],[8,1]], 1384),
    ([[[[1,1],[2,2]],[3,3]],[4,4]], 445),
    ([[[[3,0],[5,3]],[4,4]],[5,5]], 791),
    ([[[[5,0],[7,4]],[5,5]],[6,6]], 1137),
    ([[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]], 3488)
])
def test_magnitude(snailfish, expect):
    assert magnitude(snailfish) == expect


HOMEWORK = [
    [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],
    [[[5,[2,8]],4],[5,[[9,9],0]]],
    [6,[[[6,2],[5,6]],[[7,6],[4,7]]]],
    [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]],
    [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]],
    [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]],
    [[[[5,4],[7,7]],8],[[8,3],8]],
    [[9,3],[[9,9],[6,[4,9]]]],
    [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]],
    [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
]

def test_part1():
    assert part1(HOMEWORK) == 4140

def test_part2():
    assert part2(HOMEWORK) == 3993


if __name__ == "__main__":
    with open("input.txt") as f:
        input = [eval(line) for line in f.readlines()]
        print("Part 1:", part1(input))
        print("Part 2:", part2(input))
