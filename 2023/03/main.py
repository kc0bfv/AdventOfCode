#!/usr/bin/env python3

from collections import defaultdict, namedtuple
from operator import mul
from functools import reduce

DIGITS = [0,1,2,3,4,5,6,7,8,9]
DIGITS_S = [str(d) for d in DIGITS]

def product(it):
    return reduce(mul, it, 1)

"""
class Symbol:
    def __init__(self, symbol, location):
        self.symbol = symbol
        self.location = location
    def __str__(self):
        return f"{self.symbol}: {self.location}"
"""

# These are probably not an improvement...
Symbol = namedtuple("Symbol", ["symbol", "location"])
Part = namedtuple("Part", ["number", "location", "nearby_symbols"])

"""
class Part:
    def __init__(self, location):
        self.number = 0
        self.location = location
        self.nearby_symbols = set()

    def __repr__(self):
        return f"({self.number}, {self.location}, {[str(s) for s in self.nearby_symbols]})"
"""

class MapParts:
    def __init__(self, lines):
        self.matrix = list()
        for y, line in enumerate(lines):
            cur_part = None
            for x, char in enumerate(line):
                if char in DIGITS_S:
                    if cur_part is None:
                        cur_part = Part(0, (x, y), tuple())
                    cur_part = cur_part._replace(number = cur_part.number * 10 + int(char))

                    for y_off in [-1, 0, 1]:
                        for x_off in [-1, 0, 1]:
                            x_check, y_check = x + x_off, y + y_off
                            if x_check < 0 or y_check < 0:
                                continue
                            try:
                                check = lines[y_check][x_check]
                                if check not in DIGITS_S and check != '.':
                                    cur_part = cur_part._replace(nearby_symbols = 
                                        tuple(set(cur_part.nearby_symbols).union([
                                            Symbol(check, (x_check, y_check))
                                        ]))
                                    )
                            except IndexError:
                                pass
                else:
                    if cur_part is not None:
                        self.matrix.append(cur_part)
                        cur_part = None

            if cur_part is not None:
                self.matrix.append(cur_part)

    def get_parts(self):
        return [e for e in self.matrix if len(e.nearby_symbols) > 0]

    def get_gears(self):
        potential = defaultdict(set)
        for part in self.matrix:
            for symbol in part.nearby_symbols:
                if symbol.symbol == "*":
                    potential[symbol.location].add(part)
        
        actual = list()
        for loc, part_list in potential.items():
            if len(part_list) == 2:
                actual.append((loc, part_list))
        return actual

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]

    mapp = MapParts(lines)
    #print(f"{mapp.matrix}")
    #print(f"{[str(m) for m in mapp.get_parts()]}")
    part_1 = sum(m.number for m in mapp.get_parts())
    print(f"Part 1: {part_1}")

    gears = mapp.get_gears()
    ratios = [product(g.number for g in gear[1]) for gear in gears]
    print(f"Part 2: {sum(ratios)}")

if __name__ == "__main__":
    main("samp1")
    main("input")
    