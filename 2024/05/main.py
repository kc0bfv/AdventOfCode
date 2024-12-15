#!/usr/bin/env python3

from collections import defaultdict
from functools import cmp_to_key
import sys

def is_correct(line, rules):
    seen = set()
    for val in line:
        for aft in rules[val]:
            if aft in seen:
                return False
        seen.add(val)
    return True

def comparer(rules, bwrules, dos, uno):
    if dos in bwrules[uno]:
        return -1
    if dos in rules[uno]:
        return 1
    print(rules[dos], bwrules[uno])
    raise RuntimeError(f"Rule not found: {uno} {dos}")

def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    rules = defaultdict(list)
    bwrules = defaultdict(list)
    updates = list()
    rules_read = True
    for line in lines:
        if line == "":
            rules_read = False
            continue
        if rules_read:
            bef, aft = line.split("|")
            rules[int(bef)].append(int(aft))
            bwrules[int(aft)].append(int(bef))
        else:
            updates.append([int(val) for val in line.split(",")])

    correct = [is_correct(line, rules) for line in updates]
    middles = (line[int(len(line)/2)]
        for line, cor in zip(updates, correct)
        if cor)
    part1 = sum(middles)
    print(f"Part 1: {part1}")

    cmp_func = lambda u, d: comparer(rules, bwrules, u, d)
    key_func = cmp_to_key(cmp_func)
    sorteds = [sorted(line, key=key_func)
        for line, cor in zip(updates, correct)
        if not cor]
    correct2 = (is_correct(line, rules) for line in sorteds)
    middles2 = (line[int(len(line)/2)]
        for line, cor in zip(sorteds, correct2)
        if cor)
    part2 = sum(middles2)
    print(f"Part 2: {part2}")
    

if __name__ == "__main__":
    main(sys.argv[1])
