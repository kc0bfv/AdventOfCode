#!/usr/bin/env python3

from collections import defaultdict
from math import ceil
import sys

def add_pt(a, b):
    return (a[0] + b[0], a[1] + b[1])

def get_nearby(cur_pt, width, corrupts):
    delts = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    poss = (add_pt(cur_pt, delt) for delt in delts)
    inbounds = (pt for pt in poss if 0 <= pt[0] < width and 0 <= pt[1] < width)
    notcorrupt = [pt for pt in inbounds if pt not in corrupts]
    return notcorrupt

def is_reachable(all_corrupts, fallen, width):
    corrupts = set(all_corrupts[:fallen])

    st_pt = (0, 0)
    ed_pt = (width-1, width-1)

    steps_to = dict()
    steps_to[st_pt] = 0
    
    cur_step = 1
    frontier = get_nearby(st_pt, width, corrupts)
    while len(frontier) > 0:
        cur_step += 1
        new_frontier = set()

        for pt in frontier:
            nearbys = get_nearby(pt, width, corrupts)
            new_frontier.update(nb for nb in nearbys if nb not in steps_to)

        for nb in new_frontier:
            steps_to[nb] = cur_step

        frontier = new_frontier

    return ed_pt in steps_to

def main(filename, fallen, width):
    with open(filename) as infile:
        lines = (line.strip().split(",") for line in infile)
        all_corrupts = [(int(l[0]), int(l[1])) for l in lines]

    corrupts = set(all_corrupts[:fallen])

    st_pt = (0, 0)
    ed_pt = (width-1, width-1)

    steps_to = dict()
    steps_to[st_pt] = 0
    
    cur_step = 1
    frontier = get_nearby(st_pt, width, corrupts)
    while len(frontier) > 0:
        cur_step += 1
        new_frontier = set()

        for pt in frontier:
            nearbys = get_nearby(pt, width, corrupts)
            new_frontier.update(nb for nb in nearbys if nb not in steps_to)

        for nb in new_frontier:
            steps_to[nb] = cur_step

        frontier = new_frontier
    print(f"Part 1: {steps_to[ed_pt]}")

    # Part 2
    cur_try = ceil(len(all_corrupts) / 2)
    cur_width = cur_try / 2
    while cur_width >= 1:
        cur_width = ceil(cur_width)
        if is_reachable(all_corrupts, cur_try, width):
            cur_try += cur_width
        else:
            cur_try -= cur_width
        cur_width = cur_width / 2

    if is_reachable(all_corrupts, cur_try, width):
        cur_try += 1

    # Test...
    r2 = is_reachable(all_corrupts, cur_try+1, width)
    r1 = is_reachable(all_corrupts, cur_try, width)
    r0 = is_reachable(all_corrupts, cur_try-1, width)

    print(f"Part 2: {all_corrupts[cur_try-1]} {cur_try} {r1}{r0}{r2}")

if __name__ == "__main__":
    main(sys.argv[1], int(sys.argv[2]), int(sys.argv[3]))
