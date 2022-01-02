import munch
import pytest

## compiler

def compile_program(input):
    program = []
    translations = {
        'inp': '{0} = next(data)',
        'add': '{0} += {1}',
        'sub': '{0} -= {1}',
        'mul': '{0} *= {1}',
        'div': '{0} = int({0} / {1})',
        'mod': '{0} %= {1}',
        'eql': '{0} = 1 if {0} == {1} else 0'
    }
    for line in input.split('\n'):
        if line.strip() == '': continue
        instr, *args = line.strip().split()
        if instr == 'div' and args[1] == '1':
            continue
        elif instr == 'mul' and args[1] == '0':
            program.append(f'{args[0]} = 0')
        else:
            program.append(translations[instr].format(*args))

    program = "\n".join(program)
    code = compile(program, filename='compiled', mode='exec')

    def run(data):
        locals = {
            'data': iter(data),
            'w': 0,
            'x': 0,
            'y': 0,
            'z': 0
        }
        exec(code, {}, locals)
        return locals
    
    return run


## problems

def load():
  with open('input.txt') as f:
    return compile_program(f.read())


def num_to_digits(n):
    for c in str(n):
        yield ord(c) - ord('0')


def digits_to_num(ds):
    return "".join(str(d) for d in ds)


def find_digit(program, digits, pos):
    biggest = None
    smallest = None
    old = digits[pos]
    for d in range(1, 10):
        digits[pos] = d
        r = program(digits)
        # print(f'{digits}, w={r["w"]} x={r["x"]} y={r["y"]} z={r["z"]}')
        z = r['z']
        if biggest is None or z > biggest[1]:
            biggest = d, z
        if smallest is None or z < smallest[1]:
            smallest = d, z

    new = 9 if biggest[1] == smallest[1] else smallest[0]
    changed = (old != new)
    digits[pos] = new
    valid = (smallest[1] == 0)
    return changed, valid


def search_valid_serial_numbers(program):
    for default in range(1, 10):
        digits = [default] * 14
        for i in range(0, 3):
            pos = 0
            while pos < 14:
                changed, valid = find_digit(program, digits, pos)
                if valid:
                    yield digits_to_num(digits)
                if changed:
                    pos = 0
                else:
                    pos += 1

def part1(program):
    return max(search_valid_serial_numbers(program))


## tests

def test_compiler_1():
    program = compile_program("""
        inp z
        inp x
        mul z 3
        eql z x
    """)
    assert program([2, 6])['z'] == 1
    assert program([2, 7])['z'] == 0


@pytest.mark.parametrize('n', range(0,16))
def test_compiler_2(n):
    program = compile_program("""
        inp w
        add z w
        mod z 2
        div w 2
        add y w
        mod y 2
        div w 2
        add x w
        mod x 2
        div w 2
        mod w 2
    """)
    result = munch.munchify(program([n]))
    assert result.z == int(n & 1 != 0)
    assert result.y == int(n & 2 != 0)
    assert result.x == int(n & 4 != 0)
    assert result.w == int(n & 8 != 0)


def test_num_to_digits():
    digits = list(num_to_digits(12339843))
    assert digits == [1,2,3,3,9,8,4,3]


def test_digits_to_num():
    num = digits_to_num([1,2,3,3,9,8,4,3])
    assert num == '12339843'


if __name__ == "__main__":
    program = load()
    print("Part 1: ", part1(program))
