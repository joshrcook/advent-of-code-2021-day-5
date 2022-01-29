# first 3 bits = package version
# next three bits - type id


from functools import partial, reduce
import os
import sys


def pipe(*func):

    def composite(f, g):
        return lambda x: g(f(x))

    return reduce(composite, func, lambda x: x)


def open_file_in_current_dir(filename):
    return open(os.path.join(sys.path[0], filename), "r")


def read_file(filename):
    return pipe(
        open_file_in_current_dir,
        lambda x: x.read()
    )(filename)


def hex_string_to_decimal(s):
    formatted_hex = "0x" + s
    return int(formatted_hex, 16)


def lpad(to_len, char, s):
    while len(s) < to_len:
        s = char + s
    return s


def binary_string_to_decimal(bs):
    bin_string = "0b" + bs
    return int(bin_string, 2)


def decimal_to_binary_string(d):
    binary_string = str(bin(d))
    pad_zeros = partial(lpad, 4, "0")
    return pad_zeros(binary_string[2:])


def hex_char_to_binary_string(char):
    return pipe(
        hex_string_to_decimal,
        decimal_to_binary_string,
    )(char)


def hex_string_to_binary(s):
    returnval = ""
    for char in s:
        returnval += hex_char_to_binary_string(char)
    return returnval


def decimal_to_hex(n):
    return (hex(n)[2:]).upper()


def binary_string_to_hex(bs):
    return pipe(
        binary_string_to_decimal,
        decimal_to_hex,
    )(bs)


def parse_contents_to_tree(bs):
    version = binary_string_to_decimal(bs[0:3])
    type_bin = bs[3:6]
    value_bin_acc = ""
    rest = bs[6:]
    firstval = None
    while firstval != "0":
        firstval = rest[0]
        value_bin_acc += rest[1:5]
        rest = rest[5:]
    return (version,
            binary_string_to_decimal(type_bin),
            binary_string_to_decimal(value_bin_acc),
            rest
            )


def solution(filename):
    contents = read_file(filename)
    binary_contents = hex_string_to_binary(contents)
    tree = parse_contents_to_tree(binary_contents)
    return tree


# print(solution("test-input.txt"))
def build_tree(bs):
    version = binary_string_to_decimal(bs[0:3])
    type_id = binary_string_to_decimal(bs[3:6])
    rest = bs[6:]
    if type_id == 4:
        is_end = False
        value = ""
        while not is_end:
            first_char = rest[0]
            if first_char == "0":
                is_end = True
            value += rest[1:5]
        return {
            "version": version,
            "type_id": type_id,
            "value": binary_string_to_decimal(value)
        }
    else:
        length_type_id = rest[0]
        if length_type_id == "0":
            length_in_bits = binary_string_to_decimal(rest[1:16])
            rest = rest[16:]
            subpackets = rest[0:length_in_bits]
            print("subpackets", subpackets)
        else:
            num_subpackets = rest[1:12]
            print("number of subpackets",
                  binary_string_to_decimal(num_subpackets))
        return {
            "version": version,
            "type_id": type_id,
            "children": []
        }


contents = read_file("test-input.txt")
binary_contents = hex_string_to_binary(contents)
tree = build_tree(binary_contents)
print(tree)
