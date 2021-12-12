
def valid():
  return "valid", None


def corrupted(c):
  return "corrupted", c


def incomplete(stack):
  stack = "".join(stack) if type(stack) is list else stack
  return "incomplete", stack


def parse(s):
  stack = []
  for c in s:
    if c == '(':
      stack = [')', *stack]
    elif c == '[':
      stack = [']', *stack]
    elif c == '{':
      stack = ['}', *stack]
    elif c == '<':
      stack = ['>', *stack]
    elif stack != [] and c == stack[0]:
      stack = stack[1:]
    elif c in ')]}>':
      return corrupted(c)

  if stack != []:
    return incomplete(stack)
  else:
    return valid()


def part1_score(result):
  if result[0] == "corrupted":
    return {
      ')': 3,
      ']': 57,
      '}': 1197,
      '>': 25137
    }[result[1]]
  else:
    return 0


def part2_score(result):
  if result[0] == "incomplete":
    score_map = {
      ')': 1,
      ']': 2,
      '}': 3,
      '>': 4
    }
    score = 0
    for c in result[1]:
      score = (score * 5) + score_map[c]
    return score
  else:
    return 0


def part1(lines):
  scores = [part1_score(parse(line)) for line in lines]
  return sum(scores)


def part2(lines):
  scores = [part2_score(parse(line)) for line in lines]
  scores = [score for score in scores if score != 0]
  scores.sort()
  assert len(scores) % 2 == 1
  return scores[len(scores) // 2]


def test_valid_blocks():
  assert parse('()') == valid()
  assert parse('([])') == valid()
  assert parse('{()()()}') == valid()
  assert parse('[<>({}){}[([])<>]]') == valid()
  assert parse('(((((((((())))))))))') == valid()


def test_corrupted_blocks():
  assert parse('([(<{}[<>[]}>{[]{[(<()>') == corrupted('}')
  assert parse('[[<[([]))<([[{}[[()]]]') == corrupted(')')
  assert parse('[{[{({}]{}}([{[{{{}}([]') == corrupted(']')
  assert parse('[<(<(<(<{}))><([]([]()') == corrupted(')')
  assert parse('<{([([[(<>()){}]>(<<{{') == corrupted('>')


def test_incomplete_blocks():
  assert parse('[({(<(())[]>[[{[]{<()<>>') == incomplete('}}]])})]')
  assert parse('[(()[<>])]({[<{<<[]>>(') == incomplete(')}>]})')
  assert parse('(((({<>}<{<{<>}{[]{[]{}') == incomplete('}}>}>))))')
  assert parse('{<[[]]>}<{[{[{[]{()[[[]') == incomplete(']]}}]}]}>')
  assert parse('<{([{{}}[<[[[<>{}]]]>[]]') == incomplete('])}>')


def test_data():
  return [
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  ]


def test_part1():
  assert part1(test_data()) == 26397


def test_part2():
  assert part2(test_data()) == 288957


if __name__ == "__main__":
  test_valid_blocks()
  test_corrupted_blocks()
  test_incomplete_blocks()
  test_part1()
  test_part2()

  with open('input.txt') as f:
    input = f.readlines()
    print("Part 1: ", part1(input))
    print("Part 2: ", part2(input))
