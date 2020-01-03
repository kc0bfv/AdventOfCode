#!/usr/bin/env python3

import re

def dist_calc(pt1, pt2):
    return sum(abs(uno-dos) for (uno, dos) in zip(pt1, pt2))

def build_inrange_tester(bot1):
    bot1r, bot1pos = bot1
    def tester(bot_test):
        _, bottestpos = bot_test
        return bot1r >= dist_calc(bot1pos, bottestpos)
    return tester

def inrange(pt, circs):
    return len([(circr, circpos) for (circr, circpos) in circs
        if dist_calc(pt, circpos) <= circr])

def probe_point_max(closest, origin, extents, circs):
    mults = [(x, y, z) for x in (-1, 1) for y in (-1, 1) for z in (-1, 1)]
    orx, ory, orz = origin
    best_inrcnt = 0
    best_pt = (None, None, None)
    best_pt_dist_closest = None
    for x in range(orx, orx+extents):
        for y in range(ory, ory+extents):
            for z in range(orz, orz+extents):
                for xm, ym, zm in mults:
                    tmppt = (x*xm,y*ym,z*zm)
                    inrcnt = inrange(tmppt, circs)
                    if inrcnt > best_inrcnt:
                        best_inrcnt = inrcnt
                        best_pt = tmppt
                        best_pt_dist_closest = dist_calc(closest, best_pt)
                    elif inrcnt == best_inrcnt:
                        dist_close = dist_calc(closest, tmppt)
                        if dist_close < best_pt_dist_closest:
                            best_pt = tmppt
                            best_pt_dist_closest = dist_close
    return best_pt

def probe_areas_with_resolution(closest, areas, resolution, circs):
    best_pts = []
    tried_pts = set()
    to_keep = 100
    areano = 0
    for xmin,xmax,ymin,ymax,zmin,zmax in areas:
        if areano % 10 == 0:
            print("doing area {}".format(areano))
        areano += 1
        for x in range(xmin-(xmin % resolution),xmax,resolution):
            for y in range(ymin-(ymin % resolution),ymax,resolution):
                for z in range(zmin-(zmin % resolution),zmax,resolution):
                    tmppt = (x,y,z)
                    if tmppt in tried_pts:
                        continue
                    tried_pts.add(tmppt)
                    inrcnt = inrange(tmppt, circs)
                    dclc = dist_calc(closest, tmppt)
                    entry = (1000-inrcnt, dclc, tmppt)
                    if len(best_pts) < to_keep:
                        best_pts.append(entry)
                        best_pts.sort()
                    elif entry < best_pts[-1]:
                        best_pts[-1] = entry
                        best_pts.sort()

    return best_pts

def ant_walk(closest, startpt, circs):
    moves = [-1,0,1, -10,10, -100,100, -1000,1000]
    offsets = [(offx, offy, offz) for offx in moves for offy in moves
            for offz in moves]
    poss_next_pts = [(startpt[0]+offx, startpt[1]+offy, startpt[2]+offz)
            for (offx, offy, offz) in offsets]
    score_move = [(len(circs)-inrange(pt, circs), dist_calc(closest, pt), pt)
            for pt in poss_next_pts]
    score_move.sort()
    #print(score_move[0])
    return score_move[0][2]

def find_local_max(closest, startpt, circs):
    cur_pt = None
    next_pt = startpt
    i = 0
    while next_pt != cur_pt:
        cur_pt = next_pt
        next_pt = ant_walk(closest, cur_pt, circs)
        if i % 1000 == 0:
            print(i, next_pt)
        i += 1
    return cur_pt

def find_extents(circs):
    xmin, xmax, ymin, ymax, zmin, zmax = [None]*6
    for rad, pos in circs:
        if xmin is None or pos[0] < xmin:
            xmin = pos[0]
        if xmax is None or pos[0] > xmax:
            xmax = pos[0]
        if ymin is None or pos[1] < ymin:
            ymin = pos[1]
        if ymax is None or pos[1] > ymax:
            ymax = pos[1]
        if zmin is None or pos[2] < zmin:
            zmin = pos[2]
        if zmax is None or pos[2] > zmax:
            zmax = pos[2]
    return (xmin, xmax, ymin, ymax, zmin, zmax)

if __name__ == "__main__":
    with open("input day 23.txt") as f:
        lines = [line for line in f]

    patt = "pos=<(-?[0-9]*),(-?[0-9]*),(-?[0-9]*)>, r=([0-9]*)"
    groups = [re.match(patt, line).groups() for line in lines]
    inputs = [(int(r), (int(x), int(y), int(z))) for (x,y,z,r) in groups]
    
    inputs.sort()

    big_rad = inputs[-1]
    inrange_bigrad = build_inrange_tester(big_rad)

    in_range = [indat for indat in inputs if inrange_bigrad(indat)]

    print("Part1 {}".format(len(in_range)))

    best_pt = find_local_max((0,0,0), (24730014, 45567455, 22103693), inputs)
    print(best_pt, inrange(best_pt, inputs), dist_calc((0,0,0), best_pt))
    print(sum(abs(i) for i in best_pt))
    exit()
    #best_pt = probe_point_max((0,0,0), (24730017, 44601531, 23069631), 10, inputs)
    #print(best_pt)
    #print("Part2 {}".format(sum(abs(i) for i in best_pt)))

    areas = [find_extents(inputs)]
    resolution = int((areas[0][1]-areas[0][0]) / 10)
    while resolution > 1:
        result = probe_areas_with_resolution((0,0,0), areas, resolution, inputs)
        print(resolution, result)
        print(find_extents((0,(x,y,z)) for _,_,(x,y,z) in result))
        areas = [(x-resolution, x+resolution, y-resolution, y+resolution, z-resolution, z+resolution) for _, _, (x,y,z) in result]
        resolution = int(resolution / 10)

    result = probe_areas_with_resolution((0,0,0), areas, 1, inputs)
    print(1, result)

    #print("Part2 {}".format(sum(abs(i) for i in best_pt)))
