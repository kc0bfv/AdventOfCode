#!/usr/bin/env python3

from collections import defaultdict
import sys

def step(lines, cur_pos, cur_dir):
    deltas = {
        "^": (0, -1),
        "v": (0, 1),
        "<": (-1, 0),
        ">": (1, 0)
        }
    rotate = { "^": ">", ">": "v", "v": "<", "<": "^" }
    delta_pos = deltas[cur_dir]
    check_pos = tuple(sum(vals) for vals in zip(cur_pos, delta_pos))

    if not (0 <= check_pos[0] < len(lines[0]) and 0 <= check_pos[1] < len(lines)):
        # We're off the map - which is an end case
        return check_pos, cur_dir

    if lines[check_pos[1]][check_pos[0]] == ".":
        # Step that dir
        return check_pos, cur_dir
    else:
        return cur_pos, rotate[cur_dir]

def is_sim_loop(lines, cur_pos, cur_dir):
    map_ed = [list(line) for line in lines]

    uni_dir = set()

    while 0 <= cur_pos[0] < len(lines[0]) and 0 <= cur_pos[1] < len(lines):
        if ((cur_pos), cur_dir) in uni_dir:
            return True

        uni_dir.add(((cur_pos), cur_dir))

        map_ed[cur_pos[1]][cur_pos[0]] = "."

        cur_pos, cur_dir = step(lines, cur_pos, cur_dir)

        try:
            map_ed[cur_pos[1]][cur_pos[0]] = cur_dir
        except IndexError:
            pass

        #print_map(map_ed)
    
    return False

def print_map(lines):
    for line in lines:
        print("".join(line))
    print()

def main(filename):
    with open(filename) as infile:
        orig_lines = [list(line.strip()) for line in infile]

    for row, line in enumerate(orig_lines):
        try:
            col = line.index("^")
        except ValueError:
            continue
        orig_pos, orig_dir = (col, row), "^"

    orig_lines[orig_pos[1]][orig_pos[0]] = "."

    map_ed = [list(line) for line in orig_lines]
    lines = [list(line) for line in orig_lines]

    cur_pos, cur_dir = orig_pos, orig_dir

    uniques = set()

    while 0 <= cur_pos[0] < len(lines[0]) and 0 <= cur_pos[1] < len(lines):
        uniques.add(cur_pos)

        map_ed[cur_pos[1]][cur_pos[0]] = "."

        cur_pos, cur_dir = step(lines, cur_pos, cur_dir)

        try:
            map_ed[cur_pos[1]][cur_pos[0]] = cur_dir
        except IndexError:
            pass

    print(f"Part 1: {len(uniques)}")

    part2_pos = 0
    tmp_map = [list(line) for line in orig_lines]
    for unique in uniques:
        if unique == orig_pos:
            continue

        #print(f"Testing: {unique}")
        tmp_map[unique[1]][unique[0]] = "#"

        if is_sim_loop(tmp_map, orig_pos, orig_dir):
            part2_pos += 1

        tmp_map[unique[1]][unique[0]] = "."

    print(f"Part 2: {part2_pos}")

if __name__ == "__main__":
    main(sys.argv[1])
