#!/usr/bin/env python3

from collections import defaultdict
import re
import sys

def main(filename):
    with open(filename) as infile:
        line = infile.read().strip()

    pat = r"mul\((\d{1,3}),(\d{1,3})\)"
    matches = re.findall(pat, line)
    matches_ints = [(int(v0), int(v1)) for v0, v1 in matches]
    #print(matches_ints)
    print(f"Part 1: {sum(v0*v1 for v0, v1 in matches_ints)}")

    pat2 = r"mul\((\d{1,3}),(\d{1,3})\)|(do\(\))|(don't\(\))"
    matches2 = re.findall(pat2, line)
    count_em = True
    count = 0
    for m0, m1, m2, m3 in matches2:
        if m0 and m1:
            if count_em:
                count += int(m0) * int(m1)
        elif m2:
            count_em = True
        elif m3:
            count_em = False
        else:
            raise RuntimeError("Bad RE logic!")

    print(f"Part 2: {count}")

if __name__ == "__main__":
    main(sys.argv[1])
