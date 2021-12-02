#!/usr/bin/env python3

import re

def build_map(lines):
    x_lines = list()
    y_lines = list()
    ymin, ymax, xmin, xmax = 0, 0, 500, 500
    for line in lines:
        patt = "([xy])=([0-9]*), ([xy])=([0-9]*)..([0-9]*)"
        mat = re.match(patt, line.strip())
        if not mat:
            raise RuntimeError("Match fail: {}".format(line))
        let_1, scoord_1, let_2, scoord_2_1, scoord_2_2 = mat.groups()
        coord_1 = int(scoord_1)
        coord_2_1, coord_2_2 = int(scoord_2_1), int(scoord_2_2)
        if let_1 == "x" and let_2 != "y" or let_1 == "y" and let_2 != "x":
            raise RuntimeError("Letter assumption fail: {}".format(line))
        if coord_2_2 < coord_2_1:
            raise RuntimeError("Coord 2 assumption fail: {}".format(line))
        
        if let_1 == "x":
            x_lines.append((coord_1, (coord_2_1, coord_2_2)))
            if coord_1 < xmin:
                xmin = coord_1
            if coord_1 > xmax:
                xmax = coord_1
            if min(coord_2_1, coord_2_2) < ymin:
                ymin = min(coord_2_1, coord_2_2)
            if max(coord_2_1, coord_2_2) > ymax:
                ymax = max(coord_2_1, coord_2_2)
        elif let_1 == "y":
            y_lines.append((coord_1, (coord_2_1, coord_2_2)))
            if coord_1 < ymin:
                ymin = coord_1
            if coord_1 > ymax:
                ymax = coord_1
            if min(coord_2_1, coord_2_2) < xmin:
                xmin = min(coord_2_1, coord_2_2)
            if max(coord_2_1, coord_2_2) > xmax:
                xmax = max(coord_2_1, coord_2_2)
        else:
            raise RuntimeError("XY assumption fail: {}".format(line))
        
    # Scoot the sides away a little bit...
    xmin -= 2
    xmax += 2
    ymax += 2

    groundmap = [["." for i in range(xmin, xmax+1)]
            for j in range(ymin, ymax+1)]

    for (xcoord, (y_st, y_end)) in x_lines:
        for ycoord in range(y_st, y_end+1):
            groundmap[ycoord-ymin][xcoord-xmin] = "#"

    for (ycoord, (x_st, x_end)) in y_lines:
        for xcoord in range(x_st, x_end+1):
            groundmap[ycoord-ymin][xcoord-xmin] = "#"

    extents = (ymin, ymax, xmin, xmax)
    if ymin != 0:
        raise RuntimeError("ymin assumption fail: {}".format(extents))

    groundmap[0-ymin][500-xmin] = "+"

    return (extents, groundmap)

def print_groundmap(groundmap):
    for line in groundmap:
        print("".join(line))

def find_cliff_extents(groundmap, move):
    if groundmap[move[1]+1][move[0]] == ".":
        raise RuntimeError("Finding cliff extents on non-cliff")
    # Find the left edge or wall of the cliff
    tmpx, tmpy = move[0]-1, move[1]
    while groundmap[tmpy+1][tmpx] in ["#", "~"] and \
            groundmap[tmpy][tmpx] in [".", "|"]:
                tmpx -= 1
    left_edge = tmpx, tmpy
    # Find the right edge or wall of the cliff
    tmpx, tmpy = move[0]+1, move[1]
    while groundmap[tmpy+1][tmpx] in ["#", "~"] and \
            groundmap[tmpy][tmpx] in [".", "|"]:
                tmpx += 1
    right_edge = tmpx, tmpy
    return (left_edge, right_edge)

def fill_level(groundmap, left_edge, right_edge):
    new_moves = list()
    if groundmap[left_edge[1]+1][left_edge[0]] in ["#","~"] and \
            groundmap[right_edge[1]+1][right_edge[0]] in ["#","~"]:
        # Fill in water at the level
        for x in range(left_edge[0]+1, right_edge[0]):
            groundmap[left_edge[1]][x] = "~"
        # Let us fill in water at the next-level-up, maybe
        # Find and add all moves above that are |
        for x in range(left_edge[0], right_edge[0]+1):
            if groundmap[left_edge[1]-1][x] == "|":
                new_moves.append((x, left_edge[1]-1))
    else:
        # There was a break in the cliff...
        # Fill in passed-through sand at the level
        for x in range(left_edge[0]+1, right_edge[0]):
            groundmap[left_edge[1]][x] = "|"
        if groundmap[left_edge[1]][left_edge[0]] == ".":
            groundmap[left_edge[1]][left_edge[0]] = "|"
        if groundmap[right_edge[1]][right_edge[0]] == ".":
            groundmap[right_edge[1]][right_edge[0]] = "|"
        if groundmap[left_edge[1]+1][left_edge[0]] == ".":
            new_moves.append((left_edge[0], left_edge[1]))
        if groundmap[right_edge[1]+1][right_edge[0]] == ".":
            new_moves.append((right_edge[0], right_edge[1]))

    return groundmap, new_moves

def move_water(extents, groundmap, last_moves):
    (ymin, ymax, xmin, xmax) = extents
    if last_moves == []:
        if groundmap[0][500-xmin] != "+":
            raise RuntimeError("Spring died?")
        groundmap[1][500-xmin] = "|"
        last_moves = [(500-xmin, 1)]
        return groundmap, last_moves

    new_moves = list()
    for move in last_moves:
        move_was = groundmap[move[1]][move[0]]
        if move_was == "|":
            if groundmap[move[1]+1][move[0]] == ".":
                # Move down
                groundmap[move[1]+1][move[0]] = "|"
                if move[1]+1 < ymax:
                    new_moves.append((move[0], move[1]+1))
            elif groundmap[move[1]+1][move[0]] == "|":
                pass # We already calculated moves below this point
            elif groundmap[move[1]+1][move[0]] == "~":
                # Fill in sideways if possible
                left_edge, right_edge = find_cliff_extents(groundmap, move)
                groundmap, more_moves = fill_level(groundmap,
                        left_edge, right_edge)

                new_moves += more_moves
            elif groundmap[move[1]+1][move[0]] == "#":
                # Fill in sideways if possible
                left_edge, right_edge = find_cliff_extents(groundmap, move)

                groundmap, more_moves = fill_level(groundmap,
                        left_edge, right_edge)

                new_moves += more_moves
        elif move_was == "~":
            print("Already handled move: {}".format(move))
        else:
            print("Other case: {} {}".format(move, move_was))

    return groundmap, new_moves


def count_water(extents, groundmap, types=["|","~"]):
    (ymin, ymax, xmin, xmax) = extents
    newymin, newymax = 10000, 0
    for y in range(len(groundmap)):
        if "#" in groundmap[y]:
            if y < newymin:
                newymin = y
            if y > newymax:
                newymax = y
    water = 0
    for y in range(newymin, newymax+1):
        for x in range(len(groundmap[0])):
            if groundmap[y][x] in types:
                water += 1
    return water


if __name__ == "__main__":
    with open("input day 17.txt") as f:
        lines = [line.strip() for line in f]

    extents, groundmap = build_map(lines)

    print(extents)
    print_groundmap(groundmap)

    groundmap, last_moves = move_water(extents, groundmap, list())
    while last_moves:
        groundmap, last_moves = move_water(extents, groundmap, last_moves)
        print(last_moves)
        print_groundmap(groundmap)

    print(count_water(extents, groundmap))
    print(count_water(extents, groundmap, ["~"]))
