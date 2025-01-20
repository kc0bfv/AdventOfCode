#!/usr/bin/env python3

from collections import defaultdict
import sys

def get_neigh(pt, lines):
    deltas = [[-1, 0], [1, 0], [0, -1], [0, 1]]
    retlst = list()
    for delta in deltas:
        new_pt = (pt[0] + delta[0], pt[1] + delta[1])
        if 0 <= new_pt[0] < len(lines[0]) and 0 <= new_pt[1] < len(lines):
            retlst.append(new_pt)
    return retlst

def score(zero, lines):
    # BFS through the map
    score = 0
    cur_frontier = set([zero])
    cur_frontier_l = list([zero])
    feet = set()
    while len(cur_frontier) > 0:
        nxt_frontier = set()
        nxt_frontier_l = list()
        for fr_pt in cur_frontier_l:
            fr_pt_sc = lines[fr_pt[1]][fr_pt[0]]

            # If this frontier point is at a trail end, inc score and drop it
            if fr_pt_sc == 9:
                score += 1
                feet.add(fr_pt)
                continue

            # Otherwise add any valid next trail step to the new frontier
            for neigh in get_neigh(fr_pt, lines):
                if lines[neigh[1]][neigh[0]] == fr_pt_sc + 1:
                    nxt_frontier.add(neigh)
                    nxt_frontier_l.append(neigh)

        cur_frontier = nxt_frontier
        cur_frontier_l = nxt_frontier_l
    print(score, len(feet))
    return len(feet), score

def main(filename):
    with open(filename) as infile:
        lines_r = (line.strip() for line in infile)
        lines = [[int(v) for v in line] for line in lines_r]

    zeroes = list()
    for row, line in enumerate(lines):
        for col, val in enumerate(line):
            if val == 0:
                zeroes.append((col, row))

    p1scores = [score(zero, lines) for zero in zeroes]
    print(list(zip(zeroes,p1scores)))
    p1 = sum(list(zip(*p1scores))[0])
    p2 = sum(list(zip(*p1scores))[1])
    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")

if __name__ == "__main__":
    main(sys.argv[1])
