#!/usr/bin/env python3

from collections import defaultdict
import sys

def main(filename):
    with open(filename) as infile:
        lines = (line for line in infile)
        splitted = (line.split() for line in lines)
        vals = [(int(i), int(j)) for i, j in splitted]

    list1, list2 = list(zip(*vals))

    # Part 1
    list1_s, list2_s = sorted(list1), sorted(list2)
    diffs = (abs(i - j) for i, j in zip(list1_s, list2_s))
    print(f"Part 1: {sum(diffs)}")

    # Part 2
    l2dict = defaultdict(int)
    for val in list2:
        l2dict[val] += 1
    p2ans = sum(val * l2dict[val] for val in list1)
    print(f"Part 2: {p2ans}")
    

if __name__ == "__main__":
    main(sys.argv[1])
