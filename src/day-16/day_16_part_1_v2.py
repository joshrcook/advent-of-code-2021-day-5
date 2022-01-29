from math import ceil


def read(size, bits):
    returnval = bits[0:size]
    return (returnval, bits[size:])


def padleft(size, char, str):
    while len(str) < size:
        str = char + str
    return str


def bin_from_hex(hex):
    val = str(bin(int(hex, 16)))[2:]
    makelen = ceil(len(val) / 4) * 4
    return padleft(makelen, "0", val)


def dec_from_bin(bin):
    return int("0b" + bin, 2)


def make_value_object(version, type_id, value):
    return {
        "version": dec_from_bin(version),
        "type_id": dec_from_bin(type_id),
        "value": dec_from_bin(value)
    }


def make_operator_object(version, type_id, children):
    return {
        "version": dec_from_bin(version),
        "type_id": dec_from_bin(type_id),
        "children": children
    }


def parse(bits):
    version, bits = read(3, bits)
    type_id, bits = read(3, bits)
    if dec_from_bin(type_id) == 4:
        go, bits = read(1, bits)
        value, bits = read(4, bits)
        while go == "1":
            go, bits = read(1, bits)
            add, bits = read(4, bits)
            value += add
        return (make_value_object(version, type_id, value), bits)
    else:
        length_type_id, bits = read(1, bits)
        if length_type_id == "0":
            total_length_bin, bits = read(15, bits)
            total_length = dec_from_bin(total_length_bin)
            bits, _ = read(total_length, bits)
            children = []
            while len(bits) > 0:
                val, bits = parse(bits)
                children.append(val)
            return (make_operator_object(version, type_id, children), bits)

        else:
            sub_packets_bin, bits = read(11, bits)
            sub_packets = dec_from_bin(sub_packets_bin)
            children = []
            for _ in range(0, sub_packets):
                val, bits = parse(bits)
                children.append(val)
            return (make_operator_object(version, type_id, children), bits)


# TODO: write a function to sum the version numbers
i = input()
b = bin_from_hex(i)
print("b", b, len(b), ceil(len(b) / 4) * 4)
value, leftover_bits = parse(b)
print(value)
