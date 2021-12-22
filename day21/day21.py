import os

def sum_to_n(n):
    return n * (n + 1) // 2


def rollover(n, limit):
    return (n - 1) % limit + 1


class die:
    def __init__(self, sides=100):
        self._sides = sides
        self._next = 1
        self._rolls = 0

    def roll(self, n):
        self._rolls += n
        r = 0
        while n > 0:
            r += self._next
            self._next = rollover(self._next + 1, self._sides)
            n -= 1
        return r


class player:
    def __init__(self, id, start):
        self._id = id
        self._start = start
        self._pos = start
        self._score = 0

    def move(self, n):
        self._pos = rollover(self._pos + n, 10)
        self._score += self._pos       
        return self._score >= 1000

    def find_loop(self, die):
        p = player(self._id, self._start)
        pos = []
        if self._id == 2:
            die.roll(3) # for first player
        while True:
            p.move(die.roll(3))
            pos.append(p._pos)
            if len(pos) % 2 == 0:
                half = pos[:len(pos)//2]
                if pos == half + half:
                    return half
            die.roll(3) # for other player


def part1(p1start, p2start):
    p1 = player(1, p1start)
    p2 = player(2, p2start)
    d = die()
    while True:
        if p1.move(d.roll(3)):
            return p2._score * d._rolls

        if p2.move(d.roll(3)):
            return p1._score * d._rolls


def part2(p1start, p2start):
    p1 = player(1, p1start)
    p2 = player(2, p2start)
    p1_loop = p1.find_loop(die(sides=3))
    p2_loop = p2.find_loop(die(sides=3))
    print(p1_loop, p2_loop)


def test_part1():
    assert part1(4, 8) == 739785


def test_part2():
    part2(4, 8)


if __name__ == "__main__":
    test_part1()
    print("Part 1", part1(3, 5))

    test_part2()

