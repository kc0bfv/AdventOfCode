#!/usr/bin/env python3

from collections import defaultdict
from functools import reduce
from operator import mul
from math import floor, ceil
import re
import sys

class Robutt:
    def __init__(self, extents, line):
        exp = r"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)"
        matches = re.match(exp, line).groups()
        self.extents = extents
        self.p = (int(matches[0]), int(matches[1]))
        self.v = (int(matches[2]), int(matches[3]))

    def step(self, cnt):
        new_p = self.p[0] + self.v[0] * cnt, self.p[1] + self.v[1] * cnt
        self.p = new_p[0] % self.extents[0], new_p[1] % self.extents[1]

    def get_quad(self):
        # Original quad-getting algorithm that I genericized below
        x_halves = (floor(self.extents[0]/2), ceil(self.extents[0]/2))
        y_halves = (floor(self.extents[1]/2), ceil(self.extents[1]/2))
        
        quad_poss = set()
        if 0 <= self.p[0] < floor(self.extents[0]/2):
            quad_poss = {0, 2}
        elif ceil(self.extents[0]/2) <= self.p[0] < self.extents[0]:
            quad_poss = {1, 3}
        else:
            if self.p[0] != floor(self.extents[0]/2):
                raise RuntimeError("Invalid x pos")
            # Halfway...
            return None

        if 0 <= self.p[1] < floor(self.extents[1]/2):
            actual_quad = quad_poss.intersection({0, 1})
        elif ceil(self.extents[1]/2) <= self.p[1] < self.extents[1]:
            actual_quad = quad_poss.intersection({2, 3})
        else:
            if self.p[1] != floor(self.extents[1]/2):
                raise RuntimeError("Invalid y pos")
            # Halfway...
            return None

        if len(actual_quad) != 1:
            raise RuntimeError("Bad quad find")
        return actual_quad.pop()

    def get_sections(self, divs=4, drop_mids=False):
        x_parts = [i * extents[0]/divs for i in range(divs)]
        y_parts = [i * extents[1]/divs for i in range(divs)]
        x_parts.append(extents[0])
        y_parts.append(extents[1])
        
        div_laydown = [[c + (r * divs) for c in range(divs)] for r in range(divs)]

        sect_poss_x = None
        for x_ind, (st, ed) in enumerate(zip(x_parts, x_parts[1:])):
            if drop_mids and self.p[0] != 0 and self.p[0] == floor(st):
                return None
            if floor(st) <= self.p[0] < floor(ed):
                sect_poss_x = {div_laydown[r][x_ind] for r in range(divs)}

        if sect_poss_x is None:
            raise RuntimeError("sect_poss_x not found")

        sect_poss_y = None
        for y_ind, (st, ed) in enumerate(zip(y_parts, y_parts[1:])):
            if drop_mids and self.p[1] != 0 and self.p[1] == floor(st):
                return None
            if floor(st) <= self.p[1] < floor(ed):
                sect_poss_y = {div_laydown[y_ind][c] for c in range(divs)}

        if sect_poss_y is None:
            raise RuntimeError("sect_poss_y not found")

        actual_quad = sect_poss_x.intersection(sect_poss_y)

        if len(actual_quad) != 1:
            raise RuntimeError(f"Bad quad find {self.p}")
        return actual_quad.pop()

def draw(robutts, extents):
    area = [[" " for _ in range(extents[0])] for _ in range(extents[1])]
    for robutt in robutts:
        area[robutt.p[1]][robutt.p[0]] = "#"
    return area

def print_area(area):
    for line in area:
        print("".join(line))

def is_sparse(robutts):
    num_sects_across = 16
    sects = [robutt.get_sections(num_sects_across) for robutt in robutts]
    sect_cnt = [0 for _ in range(num_sects_across * num_sects_across)]
    for sect in sects:
        sect_cnt[sect] += 1

    # Threshold is sections with less than 1/4 of the robutts they should have
    # if robutts are evently spaced.
    threshold = (len(robutts) / (num_sects_across * num_sects_across)) / 4

    below_thresh = 0
    for c in sect_cnt:
        if c < threshold:
            below_thresh += 1

    # if 1/8 are below the threshold...
    if below_thresh > ((num_sects_across * num_sects_across) / 4):
        return True

    return False

def main(filename, extents):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]
    
    # Part 1
    robutts = [Robutt(extents, line) for line in lines]
    _ = [robutt.step(100) for robutt in robutts]
    #quads = [robutt.get_quad() for robutt in robutts]
    quads = [robutt.get_sections(2, True) for robutt in robutts]
    quad_cnt = [0, 0, 0, 0]
    for quad in quads:
        # Skip middles...
        if quad is None:
            continue
        quad_cnt[quad] += 1

    part1 = reduce(mul, quad_cnt)
    print(f"Part 1: {part1}")

    # Part 2
    # I don't know what the christmas tree is supposed to look like, and a
    # couple expectations I tried failed to pan out, so I'm using this
    # system of trying to find sparse patterns and only view those
    robutts = [Robutt(extents, line) for line in lines]
    for step in range(extents[0]*extents[1]*2):
        area = draw(robutts, extents)
        if is_sparse(robutts):
            print(step, "    ", "-" * (extents[0] + 10))
            print_area(area)
            print("-" * (extents[0] + 10))
        [robutt.step(1) for robutt in robutts]
    

if __name__ == "__main__":
    extents = (101, 103)
    if sys.argv[1] == "in_test.txt":
        extents = (11, 7)
    main(sys.argv[1], extents)
