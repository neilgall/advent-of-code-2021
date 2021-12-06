#!/usr/bin/env python3
from collections import defaultdict, namedtuple
import numpy as np
import os
import pytest
import re


# ----------------------------
# Model

Line = namedtuple('Line', ['xs', 'ys'])


# ----------------------------
# Parsing

LINE_RE = re.compile(r"(\d+),(\d+) -> (\d+),(\d+)")

def parse(line):
  def range(a, b):
    return np.arange(a, b+1) if a < b else np.arange(a, b-1, -1)

  m = LINE_RE.match(line)
  if m is None or len(m.groups()) != 4:
    raise Exception("bad input")
  c = [int(n) for n in m.groups()]
  xs = range(c[0], c[2])
  ys = range(c[1], c[3])
  return Line(xs, ys)


# ----------------------------
# Part 1

def is_h_or_v(line):
  return len(line.xs) == 1 or len(line.ys) == 1


def line_points(line):
  if len(line.xs) == 1:
    return ((line.xs[0], y) for y in line.ys)
  elif len(line.ys) == 1:
    return ((x, line.ys[0]) for x in line.xs)
  elif len(line.xs) == len(line.ys):
    if line.ys[1] > line.ys[0]:
      y0 = np.min(line.ys)
      return ((x, y0 + y) for y,x in enumerate(line.xs))
    else:
      yN = np.max(line.ys)
      return ((x, yN - y) for y,x in enumerate(line.xs))
  else:
    raise Exception("line must be horizontal, vertical or 45 degrees")


def count_intersection_points(lines):
  points = defaultdict(int)
  for line in lines:
    for p in line_points(line):
      points[p] += 1
  return sum(1 for p,n in points.items() if n >= 2)


def part1(lines):
  h_or_v_lines = [l for l in lines if is_h_or_v(l)]
  return count_intersection_points(h_or_v_lines)

def part2(lines):
  return count_intersection_points(lines)


# ----------------------------
# Tests

@pytest.mark.parametrize(
  'input,xs,ys', [
    ("0,9 -> 5,9", [0,1,2,3,4,5], [9]),
    ("8,0 -> 0,8", [8,7,6,5,4,3,2,1,0], [0,1,2,3,4,5,6,7,8]),
    ("9,4 -> 3,4", [9,8,7,6,5,4,3], [4]),
    ("3,4 -> 1,4", [3,2,1], [4])
  ]
)
def test_parse(input, xs, ys):
  line = parse(input)
  assert list(line.xs) == xs
  assert list(line.ys) == ys


@pytest.mark.parametrize(
  'input,h_or_v', [
    ("0,9 -> 5,9", True),
    ("9,4 -> 9,8", True),
    ("8,0 -> 0,8", False)
  ]
)
def test_is_h_or_v(input, h_or_v):
  line = parse(input)
  assert is_h_or_v(line) == h_or_v


@pytest.mark.parametrize(
  'input,points', [
    ("0,9 -> 5,9", {(0,9),(1,9),(2,9),(3,9),(4,9),(5,9)}),
    ("9,4 -> 9,8", {(9,4),(9,5),(9,6),(9,7),(9,8)}),
    ("8,0 -> 0,8", {(8,0),(7,1),(6,2),(5,3),(4,4),(3,5),(2,6),(1,7),(0,8)})
  ]
)
def test_line_points(input, points):
  line = parse(input)
  assert set(line_points(line)) == points


@pytest.fixture
def example_data():
  return [parse(line) for line in """
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2""".strip().splitlines()]

def test_part1(example_data):
  assert part1(example_data) == 5

def test_part2(example_data):
  assert part2(example_data) == 12


# ----------------------------
# Start

if __name__ == "__main__":
  with open("input.txt") as f:
    lines = [parse(line) for line in f.readlines()]
    print(f"Part 1: {part1(lines)}")
    print(f"Part 2: {part2(lines)}")
