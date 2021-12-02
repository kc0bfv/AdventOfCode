#!/usr/bin/env python3

from collections import defaultdict
import re

def print_graph(cur_dat):
    xs, ys, _, _ = list(zip(*cur_dat))
    minx, maxx = min(xs), max(xs)
    miny, maxy = min(ys), max(ys)
    print("Outline: {} {} {} {}".format(minx, maxx, miny, maxy))
    input("Press enter...")
    #xscale = (maxx - minx) / 50
    #yscale = (maxy - miny) / 50
    #newlist = [[" " for i in range(50 + 2)] for j in range(50 + 2)]
    newlist = [[" " for i in range(maxx-minx + 2)] for j in range(maxy-miny + 2)]
    for x, y, _, _ in cur_dat:
        #newlist[int((y-miny)/yscale) + 1][int((x-minx)/xscale) +1] = "#"
        newlist[(y-miny) + 1][(x-minx) +1] = "#"
    for line in newlist:
        print("".join(line))

def has_contig_line(pts, breakpt=3):
    sortedpts = sorted(pts)
    last_val = sortedpts[0]
    line_len = 0
    for pt in sortedpts[1:]:
        if pt == last_val + 1:
            line_len += 1
        else:
            line_len = 0
        last_val = pt
        if line_len > breakpt:
            return True

def no_message(cur_dat):
    xptdct = defaultdict(list)
    yptdct = defaultdict(list)
    for x,y,_,_ in cur_dat:
        xptdct[x].append(y)
        yptdct[y].append(x)
    # look for a line...
    for ypts in xptdct.values():
        if has_contig_line(ypts):
            return False
    for xpts in yptdct.values():
        if has_contig_line(xpts):
            return False
    return True

def out_of_extents(xextents, yextents, cur_dat, min_in = None):
    if not min_in:
        min_in = len(cur_dat)/2
    count_in = len(cur_dat)
    for x,y,_,_ in cur_dat:
        if x < xextents[0] or x > xextents[1] or y < yextents[0] or y > yextents[1]:
            count_in -= 1
        if count_in < min_in:
            return True
    return False

if __name__ == "__main__":
    with open("input day 10.txt") as f:
        inlines = [line for line in f]

    pos_re = re.compile("position=<([ \-0-9]*), ([ \-0-9]*)> velocity=<([ \-0-9]*), ([ \-0-9]*)>")
    parsed = (pos_re.search(line) for line in inlines)
    in_str = (pt.groups() for pt in parsed)
    indat = [[int(a) for a in pt] for pt in in_str]

    initialxs, initialys, _, _ = list(zip(*indat))
    xextents = (min(initialxs), max(initialxs))
    yextents = (min(initialys), max(initialys))

    cur_dat = [[a for a in b] for b in indat]

    seconds = 0
    keep_going = "y"
    skip_one = 0
    while keep_going[0] in ["y", "Y"]:
        while no_message(cur_dat) or skip_one > 0:
        #while not (10270 < seconds < 10290) or skip_one:
            new_dat = [[x+vx, y+vy, vx, vy] for (x,y,vx,vy) in cur_dat]
            cur_dat = new_dat
            seconds += 1
            if skip_one > 0:
                skip_one -= 1

            if out_of_extents(xextents, yextents, cur_dat):
                print("None are still in extents {}".format(seconds))
                input("Pausing")

        print_graph(cur_dat)
        keep_going = input("Keep going (y/n)? {} ".format(seconds))
        skip_one = 1
