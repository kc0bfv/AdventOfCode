#!/usr/bin/env python3

from collections import defaultdict
import sys

def is_safe(line):
    # Determine inc/dec
    count = 3
    is_inc_cnt = round(
        sum(1 if line[ind+1] > line[ind] else 0 for ind in range(count))
        / count
        )
    is_inc = is_inc_cnt == 1

    # Determine if this is line must be increasing or decreasing
    #is_inc = line[1] > line[0]

    for ind, (val1, val2) in enumerate(zip(line[0:], line[1:])):
        if (val2 > val1) != is_inc:
            return ind, False
        if abs(val2-val1) < 1 or abs(val2-val1) > 3:
            return ind, False
    return None, True


def main(filename):
    with open(filename) as infile:
        lines = (line for line in infile)
        splitted = (line.split() for line in lines)
        values = [[int(val) for val in line] for line in splitted]

    # Part 1
    safe_un = [is_safe(line) for line in values]
    safe_count = sum(1 if safe else 0 for _, safe in safe_un)
    print(f"Part 1: {safe_count}")

    # Part 2
    safe_count_2 = safe_count
    for (ind, safe), line in zip(safe_un, values):
        if safe:
            continue
        linea, lineb, linec = list(line), list(line), list(line)
        del linea[ind]
        del lineb[ind+1]
        _, lineasafe = is_safe(linea)
        _, linebsafe = is_safe(lineb)
        #print(lineasafe or linebsafe, ind, line)
        if lineasafe or linebsafe:
            safe_count_2 += 1

    print(f"Part 2: {safe_count_2}")

if __name__ == "__main__":
    main(sys.argv[1])
