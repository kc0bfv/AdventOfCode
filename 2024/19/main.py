#!/usr/bin/env python3

from collections import defaultdict
import sys

from queue import PriorityQueue
from dataclasses import dataclass, field
from typing import Any

@dataclass(order = True)
class PrioritizedPatt:
    priority: int
    patt: Any=field(compare = False)
    method: Any=field(compare = False)

class PPFactory:
    def __init__(self, patt):
        self.patt = patt
        self.len_patt = len(patt)
    def new_item(self, cur_build, prev_patt=None, item=None):
        method = (() if prev_patt is None else prev_patt.method) + \
           (() if item is None else (item, ))
        return PrioritizedPatt(self.len_patt - len(cur_build), cur_build, method)
        #return PrioritizedPatt(len(cur_build), cur_build, method)

def build_patt(patt, available):
    ppf = PPFactory(patt)
    tried = defaultdict(int)
    frontier = PriorityQueue()
    frontier.put(ppf.new_item(patt))
    tried[patt] = 1

    #print(f"Trying {patt}")

    solutions = set()
    solutions_count = 0

    while not frontier.empty():
        entry = frontier.get(block=False)
        #print(f"{len(entry.patt)} {entry.patt} {entry.method} {tried[entry.patt]}")
        for avail in available:
            if entry.patt.startswith(avail):
                #print("succ", entry.patt, avail)
                new_patt = entry.patt[len(avail):]
                new_pp = ppf.new_item(new_patt, entry, avail)

                if new_pp.patt not in tried:
                    frontier.put(new_pp)

                tried[new_pp.patt] += tried[entry.patt]

                if len(new_pp.patt) == 0:
                    solutions.add(new_pp.method)
                    solutions_count += 1
            else:
                #print("fail", entry.patt, avail)
                pass


    #print("END", tried[""], solutions_count, solutions)
    return tried[""]
                

def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]
    available = [a.strip() for a in lines[0].split(",")]
    to_make = lines[2:]

    made = [build_patt(t, available) for t in to_make]
    part1 = sum(1 for m in made if m != 0)
    print(f"Part 1: {part1}")

    part2 = sum(made)
    print(f"Part 2: {part2}")

if __name__ == "__main__":
    main(sys.argv[1])
