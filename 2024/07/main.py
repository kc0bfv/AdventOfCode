#!/usr/bin/env python3

from collections import defaultdict
from operator import mul, add
import sys

def find_opers(conc, cal, val_list):
    val, opers = recur(conc, cal, val_list, 0, 0, add, "", "")

    if val == cal:
        return opers
    else:
        return None

def con(uno, dos):
    return int(str(uno) + str(dos))

def recur(conc, cal, val_list, ind, accum, func, opers, next_o):
    if ind >= len(val_list):
        #print(accum, opers, val_list)
        return accum, opers

    accum = func(accum, val_list[ind])
    opers += next_o

    if accum > cal:
        return accum, None


    sval, sopers = recur(conc, cal, val_list, ind+1, accum, add, opers, "S")
    if sval == cal:
        return sval, sopers

    if conc:
        cval, copers = recur(conc, cal, val_list, ind+1, accum, con, opers, "C")

        if cval == cal:
            return cval, copers

    mval, mopers = recur(conc, cal, val_list, ind+1, accum, mul, opers, "M")
    return mval, mopers

def main(filename):
    with open(filename) as infile:
        lines = (line.split(":") for line in infile)
        parts = ((int(cal), vals) for cal, vals in lines)
        cals, vals_str = zip(*parts)
        vals = [[int(val) for val in line.strip().split()]
            for line in vals_str
            ]

    opers = [find_opers(False, cal, val_list) for cal, val_list in zip(cals, vals)]
    part1 = sum(cal if oper is not None else 0 for cal,oper in zip(cals, opers))
    print(f"Part 1: {part1}")

    opers = [find_opers(True, cal, val_list) for cal, val_list in zip(cals, vals)]
    part2 = sum(cal if oper is not None else 0 for cal,oper in zip(cals, opers))
    print(f"Part 2: {part2}")

if __name__ == "__main__":
    main(sys.argv[1])
