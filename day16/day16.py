import pytest

HEX_TO_BINARY = {
    '0': '0000',
    '1': '0001',
    '2': '0010',
    '3': '0011',
    '4': '0100',
    '5': '0101',
    '6': '0110',
    '7': '0111',
    '8': '1000',
    '9': '1001',
    'A': '1010',
    'B': '1011',
    'C': '1100',
    'D': '1101',
    'E': '1110',
    'F': '1111'
}

def binary_to_int(bin):
    i = 0
    for c in bin:
        i = (i * 2) + (1 if c == '1' else 0)
    return i


class decoder:
    def __init__(self, hex_str):
        self._bits = "".join(HEX_TO_BINARY[x] for x in hex_str.strip())

    def read_bits(self, num_bits):
        bits = self._bits[:num_bits]
        self._bits = self._bits[num_bits:]
        return bits

    def has_more(self):
        return self._bits != ''

    def read_int(self, num_bits):
        return binary_to_int(self.read_bits(num_bits))

    def read_all_bits(self):
        bits = self._bits
        self._bits = ''
        return bits

    def sub_decoder(self, num_bits):
        sub = decoder('')
        sub._bits = self.read_bits(num_bits)
        return sub


def packet_decoder(d):
    version = d.read_int(3)
    type_id = d.read_int(3)
    if type_id == 4:
        literal = 0
        more = True
        while more:
            more = d.read_bits(1) == '1'
            literal = (literal << 4) | d.read_int(4)
        yield (version, type_id, literal)
    else:
        length_type_id = d.read_int(1)
        yield (version, type_id, length_type_id)
        if length_type_id == 0:
            length = d.read_int(15)
            sd = d.sub_decoder(length)
            while sd.has_more():
                yield from packet_decoder(sd)
        else:
            num_packets = d.read_int(11)
            for i in range(num_packets):
                yield from packet_decoder(d)


def version_sums(hex):
    d = decoder(hex)
    return sum(v for (v,t,n) in packet_decoder(d))


@pytest.mark.parametrize('binary,integer', [
    ('0', 0),
    ('1', 1),
    ('0001', 1),
    ('1001', 9),
    ('000111000111001', 0xE39)
])
def test_binary_to_int(binary, integer):
    assert binary_to_int(binary) == integer


def test_decoder():
    d = decoder("D2FE28")
    assert d.read_bits(3) == '110'
    assert d.read_bits(3) == '100'
    assert d.read_bits(5) == '10111'
    assert d.read_bits(5) == '11110'
    assert d.read_bits(5) == '00101'
    assert d.read_all_bits() == '000'

@pytest.mark.parametrize('hex,packets',[
    ('D2FE28', [(6, 4, 2021)]),
    ('38006F45291200', [(1, 6, 0), (6, 4, 10), (2, 4, 20)]),
    ('EE00D40C823060', [(7, 3, 1), (2, 4, 1), (4, 4, 2), (1, 4, 3)])
])
def test_packet_decoder(hex, packets):
    d = decoder(hex)
    ps = list(packet_decoder(d))
    assert ps == packets
    

@pytest.mark.parametrize('hex,sum',[
    ('8A004A801A8002F478', 16),
    ('620080001611562C8802118E34', 12),
    ('C0015000016115A2E0802F182340', 23),
    ('A0016C880162017C3686B18A3D4780', 31)
])
def test_version_sums(hex, sum):
    assert version_sums(hex) == sum

if __name__ == "__main__":
    with open('input.txt') as f:
        input = f.read()
        print("Part 1: ", version_sums(input))
