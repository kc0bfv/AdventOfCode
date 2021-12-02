#!/usr/bin/env python3

SURROFF = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1),
        (1, 0), (1, 1)]

def count_surr(formap, x, y):
    surr = {"|": 0, "#": 0, ".": 0}
    for (xoff, yoff) in SURROFF:
        if 0 <= x+xoff < len(formap[0]) and 0 <= y+yoff < len(formap):
            surr[formap[y+yoff][x+xoff]] += 1
    return surr

def next_gen(formap):
    newmap = [["@" for _ in line] for line in formap]
    for y in range(len(formap)):
        for x in range(len(formap[0])):
            surr_dict = count_surr(formap, x, y)
            if formap[y][x] == ".":
                if surr_dict["|"] >= 3:
                    newmap[y][x] = "|"
                else:
                    newmap[y][x] = "."
            elif formap[y][x] == "|":
                if surr_dict["#"] >= 3:
                    newmap[y][x] = "#"
                else:
                    newmap[y][x] = "|"
            elif formap[y][x] == "#":
                if surr_dict["#"] >= 1 and surr_dict["|"] >= 1:
                    newmap[y][x] = "#"
                else:
                    newmap[y][x] = "."
            else:
                raise RuntimeError("Unknown acre {} {}".format(x, y))
    return newmap

def print_map(formap):
    for line in formap:
        print("".join(line))

def count_all(formap):
    surr = {"|": 0, "#": 0, ".": 0}
    for y in range(len(formap)):
        for x in range(len(formap[0])):
            surr[formap[y][x]] += 1
    return surr

if __name__ == "__main__":
    with open("input day 18.txt") as f:
        inlines = [line.strip() for line in f]

    formap = inlines

    prev_maps = [formap]

    for i in range(1, 600):
        formap = next_gen(formap)
        #print_map(formap)
        allcount = count_all(formap)
        print(i, allcount, allcount["|"]*allcount["#"], prev_maps.index(formap) if formap in prev_maps else 0)
        prev_maps.append(formap)
    print_map(formap)
    allcount = count_all(formap)
    print(allcount, allcount["|"]*allcount["#"])
