#!/usr/bin/env python3

from collections import defaultdict
import sys

def pt_add(a, b):
    return (a[0] + b[0], a[1] + b[1])

def taxi_dist(a, b):
    return abs(b[0]-a[0]) + abs(b[1]-a[1])

def gen_pathlens(lines, st_pt, ed_pt):
    pathlens = dict()
    move_dels = [(-1, 0), (1, 0), (0, -1), (0, 1)]

    cur_pt = ed_pt
    cur_len = 0
    pathlens[cur_pt] = cur_len
    while cur_pt != st_pt:
        all_moves = (pt_add(move_del, cur_pt) for move_del in move_dels)
        valid_moves = (move for move in all_moves if lines[move[1]][move[0]] != "#")
        not_prev_moves = tuple(move for move in valid_moves if move not in pathlens)
        if len(not_prev_moves) > 1:
            raise RuntimeError(f"More than one move!: {not_prev_moves}")

        cur_pt = not_prev_moves[0]
        cur_len += 1
        pathlens[cur_pt] = cur_len
    return pathlens

def find_cheats(lines, pathlens, st_pt, ed_pt, dist):
    cheats = defaultdict(int)
    move_dels = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for path_pt in pathlens:
        for move_x in range(path_pt[0] - dist, path_pt[0] + dist + 1):
            for move_y in range(path_pt[1] - dist, path_pt[1] + dist + 1):
                dest = (move_x, move_y)

                taxi = taxi_dist(path_pt, dest)
                if dest not in pathlens or taxi > dist:
                    continue

                saved = pathlens[path_pt] - taxi - pathlens[dest]
                if saved > 0:
                    #print(dest, path_pt, saved)
                    cheats[saved] += 1
    return cheats
                    

def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]
    st_pt = [(line.index("S"), y) for y, line in enumerate(lines) if "S" in line][0]
    ed_pt = [(line.index("E"), y) for y, line in enumerate(lines) if "E" in line][0]
    pathlens = gen_pathlens(lines, st_pt, ed_pt)
    cheats = find_cheats(lines, pathlens, st_pt, ed_pt, 2)
    over_100 = [cnt for saved, cnt in cheats.items() if saved >= 100]
    print(f"Part 1: {sum(over_100)}")

    cheats = find_cheats(lines, pathlens, st_pt, ed_pt, 20)
    over_100 = [cnt for saved, cnt in cheats.items() if saved >= 100]
    print(f"Part 2: {sum(over_100)}")

if __name__ == "__main__":
    main(sys.argv[1])
