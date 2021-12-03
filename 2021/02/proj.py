#!/usr/bin/env python3

from sys import argv

if __name__ == "__main__":
    lines = (line.strip() for line in open(argv[1], "r").readlines())
    part_parse = (line.split() for line in lines)
    parsed = [(direc, int(dist)) for (direc, dist) in part_parse]

    horiz = 0
    vert = 0

    for direc, dist in parsed:
        if direc == "forward": horiz += dist
        if direc == "down": vert += dist
        if direc == "up": vert -= dist

    print("Part 1: {} {} {}".format(horiz, vert, vert*horiz))

    horiz = 0
    vert = 0
    aim = 0

    for direc, dist in parsed:
        if direc == "forward":
            horiz += dist
            vert += aim * dist
        if direc == "down": aim += dist
        if direc == "up": aim -= dist

    print("Part 2: {} {} {}".format(horiz, vert, vert*horiz))
