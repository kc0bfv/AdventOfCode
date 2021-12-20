#!/usr/bin/env python3

import sys

def gen_cost(pos, lst):
    return sum(abs(val-pos) for val in lst)

def gen_cost2(pos, lst):
    return sum(sum_to_n(abs(val-pos)) for val in lst)

def sum_to_n(n):
    return int(.5 * n * (n+1))

if __name__ == "__main__":
    data = [int(val) for val in open(sys.argv[1], "r").read().split(",")]

    min_found_cost = None
    min_found_pos = None
    min_found_cost2 = None
    min_found_pos2 = None
    for pos in range(min(data), max(data)+1):
        cost = gen_cost(pos, data)
        cost2 = gen_cost2(pos, data)
        if min_found_cost is None or cost < min_found_cost:
            min_found_cost = cost
            min_found_pos = pos
        if min_found_cost2 is None or cost2 < min_found_cost2:
            min_found_cost2 = cost2
            min_found_pos2 = pos
    print("Part 1: {} cost {}".format(min_found_pos, min_found_cost))
    print("Part 2: {} cost {}".format(min_found_pos2, min_found_cost2))


