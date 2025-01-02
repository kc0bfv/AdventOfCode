#!/usr/bin/env python3

from collections import defaultdict
import sys

def tuplus(a, b):
    return a[0] + b[0], a[1] + b[1]

def recur_move(floor, pos, direc, actually_move, partner_move=False):
    # This is gonna modify the input floor, BTW
    if not (0 <= pos[0] < len(floor[0]) and 0 <= pos[1] < len(floor)):
        raise RuntimeError("Went off map?")
    elif floor[pos[1]][pos[0]] == "#":
        return floor, False, pos
    elif floor[pos[1]][pos[0]] == ".":
        return floor, True, pos
    elif floor[pos[1]][pos[0]] in ["O", "@"]:
        new_pos = tuplus(pos, direc)
        floor, move_allowed, _ = recur_move(floor, new_pos, direc, actually_move)
        ret_pos = pos
        if move_allowed and actually_move:
            floor[new_pos[1]][new_pos[0]] = floor[pos[1]][pos[0]]
            floor[pos[1]][pos[0]] = "."
            ret_pos = new_pos
        return floor, move_allowed, ret_pos
    elif floor[pos[1]][pos[0]] in ["[", "]"]:
        partner_direc_dict = {"[": (1, 0), "]": (-1, 0)}
        partner_direc = partner_direc_dict[floor[pos[1]][pos[0]]]

        new_pos = tuplus(pos, direc)
        floor, move_allowed, _ = recur_move(floor, new_pos, direc, actually_move)

        ret_pos = pos
        if move_allowed and actually_move:
            floor[new_pos[1]][new_pos[0]] = floor[pos[1]][pos[0]]
            floor[pos[1]][pos[0]] = "."
            ret_pos = new_pos

        move_allowed_2 = True
        # If we're not already trying the "partner" (]),
        # and this isn't a > or < move, move it too
        if move_allowed and direc[1] != 0 and not partner_move:
            new_pos_2 = tuplus(pos, partner_direc)
            floor, move_allowed_2, _ = recur_move(floor, new_pos_2, direc, actually_move, True)

        return floor, move_allowed and move_allowed_2, ret_pos

def move(floor, robot_pos, instr):
    if floor[robot_pos[1]][robot_pos[0]] != "@":
        raise RuntimeError("Wrong robot pos")

    direc_dict = {"<": (-1, 0), ">": (1, 0), "^": (0, -1), "v": (0, 1)}
    direc = direc_dict[instr]

    floor, move_allowed, _ = recur_move(floor, robot_pos, direc, False)
    if move_allowed:
        floor, move_allowed, robot_pos = recur_move(floor, robot_pos, direc, True)
        if not move_allowed:
            raise RuntimeError("Unexpected move failure")
    return floor, robot_pos

def print_floor(floor):
    print("\n".join("".join(line) for line in floor))

def score_floor(floor):
    score = 0
    for row, line in enumerate(floor):
        for col, char in enumerate(line):
            if char in ["O", "["]:
                score += col + (row * 100)
    return score

def p2_expand_lines(lines):
    char_lookup = {"#": "##", ".": "..", "O": "[]", "@": "@."}
    return ["".join(char_lookup[char] for char in line) for line in lines]

def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    sep = None
    for ind, line in enumerate(lines):
        if line == "":
            sep = ind
    if sep is None:
        raise RuntimeError("Separator not found")

    floor = [list(line) for line in lines[0:sep]]
    robot_pos = [(line.index("@"), ind)
        for ind, line in enumerate(lines)
        if "@" in line][0]
    instrs = "".join(lines[sep+1:])
    
    print(f"Move 0")
    print_floor(floor)
    for ind, instr in enumerate(instrs):
        floor, robot_pos = move(floor, robot_pos, instr)
        #print()
        #print(f"Move {ind+1}")
        #print_floor(floor)

    part1 = score_floor(floor)
    print(f"Part 1: {part1}")

    # Part 2
    p2lines = p2_expand_lines(lines[0:sep])
    floor = [list(line) for line in p2lines[0:sep]]
    robot_pos = [(line.index("@"), ind)
        for ind, line in enumerate(p2lines)
        if "@" in line][0]
    
    print(f"Move 0")
    print_floor(floor)
    for ind, instr in enumerate(instrs):
        floor, robot_pos = move(floor, robot_pos, instr)
        #print()
        #print(f"Move {ind+1}")
        #print_floor(floor)

    part2 = score_floor(floor)
    print(f"Part 2: {part2}")

if __name__ == "__main__":
    main(sys.argv[1])
