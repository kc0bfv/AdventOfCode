#!/usr/bin/env python3

def read(filename):
    with open(filename, "r") as fin:
        tobmap = [[char for char in line.strip()] for line in fin]

    return tobmap

def count_trees(tobmap, rt, dn):
    cr_rt, cr_dn = 0, 0
    cr_cnt = 0
    height = len(tobmap)
    width = len(tobmap[0])
    while cr_dn < height:
        if tobmap[cr_dn][cr_rt % width] == "#":
            cr_cnt += 1
        else:
            if tobmap[cr_dn][cr_rt % width] != ".":
                print(cr_dn, cr_rt, tobmap[cr_dn][cr_rt % width])

        cr_rt += rt
        cr_dn += dn

    return cr_cnt

if __name__ == "__main__":
    tobmap = read("input.txt")
    print(count_trees(tobmap, 3, 1))
    print(
        count_trees(tobmap, 1, 1) * 
        count_trees(tobmap, 3, 1) * 
        count_trees(tobmap, 5, 1) * 
        count_trees(tobmap, 7, 1) * 
        count_trees(tobmap, 1, 2)
        )
