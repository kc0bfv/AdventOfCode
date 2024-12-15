#!/usr/bin/env python3

from collections import defaultdict
import itertools
from string import ascii_letters, digits
import sys

def build_antinodes(ant_list):
    antinodes = set()
    for ind, ant1 in enumerate(ant_list):
        for ant2 in ant_list[ind+1:]:
            dist = (ant2[0] - ant1[0], ant2[1] - ant1[1])
            anti1 = (ant1[0] - dist[0], ant1[1] - dist[1])
            anti2 = (ant2[0] + dist[0], ant2[1] + dist[1])
            antinodes.add(anti1)
            antinodes.add(anti2)
    return antinodes

def build_antinodes_2(ant_list, maxcol, maxrow):
    antinodes = set()
    for ind, ant1 in enumerate(ant_list):
        for ant2 in ant_list[ind+1:]:
            dist = (ant2[0] - ant1[0], ant2[1] - ant1[1])
            
            for cur_loc, direc in [(ant1, -1), (ant2, 1)]:
                while 0 <= cur_loc[0] < maxcol and 0 <= cur_loc[1] < maxrow:
                    antinodes.add(cur_loc)
                    cur_loc = (cur_loc[0] + (direc*dist[0]), cur_loc[1] + (direc*dist[1]))

    return antinodes

def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    allowed_ant = ascii_letters + digits
    antennas = defaultdict(list)
    for row, line in enumerate(lines):
        for col, char in enumerate(line):
            if char in allowed_ant:
                antennas[char].append((col, row))

    max_row, max_col = len(lines), len(lines[0])

    antinodes = {char: build_antinodes(antennas[char]) for char in antennas.keys()}
    unique_antinodes = {item for item in itertools.chain(*antinodes.values())}
    on_map_antinodes = {(col, row) for (col, row) in unique_antinodes
        if 0 <= col < max_col and 0 <= row < max_row}

    print(f"Part 1: {len(on_map_antinodes)}")

    antinodes = {char: build_antinodes_2(antennas[char], max_col, max_row)
        for char in antennas.keys()}
    unique_antinodes = {item for item in itertools.chain(*antinodes.values())}
    on_map_antinodes = {(col, row) for (col, row) in unique_antinodes
        if 0 <= col < max_col and 0 <= row < max_row}

    print(f"Part 2: {len(on_map_antinodes)}")


if __name__ == "__main__":
    main(sys.argv[1])
