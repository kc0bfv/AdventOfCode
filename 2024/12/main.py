#!/usr/bin/env python3

from collections import defaultdict
import sys

# Breadth first region building
def build_reg(lines, col, row):
    deltas = [[0,1],[0,-1],[-1,0],[1,0]]

    reg = set([(col, row)])
    front = set(reg)
    while len(front) > 0:
        new_front = set()
        for col, row in front:
            # For each location around this location...
            for d_col, d_row in deltas:
                new_col, new_row = col + d_col, row + d_row
                # Don't allow negative locations.
                if 0 > new_col or 0 > new_row:
                    continue
                # If this loc is already in the region, move on
                if (new_col, new_row) in reg:
                    continue
                # If the loc is valid...
                try:
                    # and if the loc has the same char...
                    if lines[row][col] == lines[new_row][new_col]:
                        # add it to the region and search it next time.
                        reg.add((new_col, new_row))
                        new_front.add((new_col, new_row))
                    # Otherwise just keep moving on
                except IndexError:
                    pass
        front = new_front
    return reg
                
def get_perim(lines, col, row):
    deltas = [[0,1],[0,-1],[-1,0],[1,0]]
    perim = 0
    for d_col, d_row in deltas:
        new_col, new_row = col + d_col, row + d_row
        # If this new loc is off the map, then that edge is a perimeter
        if not (0 <= new_col < len(lines[0]) and 0 <= new_row < len(lines)):
            perim += 1
        else:
            if not lines[row][col] == lines[new_row][new_col]:
                perim += 1
    return perim

def get_perim_parts(lines, col, row):
    deltas = [(0,1),(0,-1),(-1,0),(1,0)]
    perim = set()
    for d_col, d_row in deltas:
        new_col, new_row = col + d_col, row + d_row
        # If this new loc is off the map, then that edge is a perimeter
        if not (0 <= new_col < len(lines[0]) and 0 <= new_row < len(lines)):
            perim.add((col, row, (d_col, d_row)))
        else:
            if not lines[row][col] == lines[new_row][new_col]:
                perim.add((col, row, (d_col, d_row)))
    return perim

def get_sides(lines, region):
    adj_lookup = {
        (0, 1): [(-1, 0), (1, 0)],
        (0, -1): [(-1, 0), (1, 0)],
        (1, 0): [(0, -1), (0, 1)],
        (-1, 0): [(0, -1), (0, 1)],
    }

    # Get all edges on the perimeter
    # Each part of perim_parts will be (col, row, (d_col, d_row))
    # Where the d_ ones are the direction/delta to the relevant edge
    perim_parts = set()
    for col, row in region:
        perim_parts.update(get_perim_parts(lines, col, row))

    # Walk every perimeter point to accumulate that total edge
    walked_perim = set()
    sides = list()
    for cur_perim in perim_parts:
        if cur_perim in walked_perim:
            continue

        # Build a new side, walking along it both directions until it ends
        side = set([cur_perim])
        walked_perim.add(cur_perim)

        # Look up the delta part to see which direction to check next
        col, row, perim_delt = cur_perim
        adjacents = adj_lookup[perim_delt]

        # In both directions...
        for adj_d_col, adj_d_row in adjacents:
            cur_col, cur_row = col, row
            keep_going = True
            while keep_going:
                cur_col, cur_row = cur_col + adj_d_col, cur_row + adj_d_row
                perim_to_check = (cur_col, cur_row, perim_delt)
                if perim_to_check in perim_parts:
                    side.add(perim_to_check)
                    if perim_to_check in walked_perim:
                        raise RuntimeError("Failure walking somewhere")
                    walked_perim.add(perim_to_check)
                else:
                    keep_going = False
        sides.append(side)
    return sides


def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    # Determine what the regions are
    in_a_reg = set()
    regions = list()
    for row, line in enumerate(lines):
        for col, char in enumerate(line):
            if (col, row) in in_a_reg:
                continue
            reg = build_reg(lines, col, row)
            regions.append(reg)
            in_a_reg.update(reg)

    print(len(regions))

    # Build a perimeter value for every point
    perim_vals = [
        [get_perim(lines, col, row) for col, _ in enumerate(line)]
        for row, line in enumerate(lines)]

    reg_perims = [sum(perim_vals[loc[1]][loc[0]] for loc in reg) for reg in regions]
    reg_areas = [len(reg) for reg in regions]
    reg_costs = [perim*area for perim, area in zip(reg_perims, reg_areas)]

    for p, a, c in zip(reg_perims, reg_areas, reg_costs):
        print(f"{p} * {a} = {c}")

    print(f"Part 1: {sum(reg_costs)}")

    all_sides = [get_sides(lines, reg) for reg in regions]
    reg_sides = [len(sides) for sides in all_sides]
    reg_costs_2 = [sides*area for sides, area in zip(reg_sides, reg_areas)]
    for p, a, c in zip(reg_sides, reg_areas, reg_costs_2):
        print(f"{p} * {a} = {c}")

    print(f"Part 2: {sum(reg_costs_2)}")

if __name__ == "__main__":
    main(sys.argv[1])
