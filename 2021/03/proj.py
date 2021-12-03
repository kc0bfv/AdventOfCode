#!/usr/bin/env python3

from sys import argv

def count_bits(pos, lines):
    zeroes, ones = 0, 0
    for line in lines:
        if line[pos] == "0":
            zeroes += 1
        else:
            ones += 1

    return (zeroes, ones)

def get_rating(lines, comp_func):
    bit_count = len(lines[0])

    lines_remaining = list(lines)

    for pos in range(bit_count):
        zeroes, ones = count_bits(pos, lines_remaining)
        keeper = comp_func(zeroes, ones)
        
        lines_remaining = [line for line in lines_remaining if line[pos] == keeper]

        if len(lines_remaining) == 1:
            return lines_remaining[0]

if __name__ == "__main__":
    lines = [line.strip() for line in open(argv[1]).readlines()]

    bit_count = len(lines[0])
    gamma, epsilon = "", ""

    for pos in range(bit_count):
        zeroes, ones = count_bits(pos, lines)
        if zeroes > ones:
            gamma += "0"
            epsilon += "1"
        else:
            gamma += "1"
            epsilon += "0"

    gi, ei = int(gamma, 2), int(epsilon, 2)
    print("Part 1: g {} {} e {} {} {}".format(gamma, gi, epsilon, ei, gi*ei))

    o2_r = get_rating(lines, lambda z, o: "0" if z > o else "1")
    co2_r = get_rating(lines, lambda z, o: "1" if z > o else "0")

    o2_i, co2_i = int(o2_r, 2), int(co2_r, 2)
    print("Part 2: o2 {} {} co2 {} {} {}".format(o2_r, o2_i, co2_r, co2_i, o2_i * co2_i))
