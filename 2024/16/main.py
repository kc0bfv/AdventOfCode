#!/usr/bin/env python3

from collections import defaultdict, namedtuple
from itertools import chain
from enum import Enum
import sys

Point = namedtuple("Point", ["x", "y"])
Pos = namedtuple("Pos", ["pt", "dir"])

class Direction(Enum):
    EAST = 0
    SOUTH = 1
    WEST = 2
    NORTH = 3
    def rot_cw(self):
        return self.__class__((self.value + 1) % 4)
    def rot_ccw(self):
        return self.__class__((self.value - 1) % 4)
    def rot_180(self):
        return self.__class__((self.value + 2) % 4)
    def fwd_delta(self):
        return {
            Direction.EAST: Point(1, 0),
            Direction.SOUTH: Point(0, 1),
            Direction.WEST: Point(-1, 0),
            Direction.NORTH: Point(0 ,-1),
        }[self]

def add_pts(a, b):
    return Point(a.x + b.x, a.y + b.y)

def list_moves(lines, pos):
    moves_a = [
        (Pos(add_pts(pos.pt, pos.dir.fwd_delta()), pos.dir), 1),
        (Pos(pos.pt, pos.dir.rot_cw()), 1000),
        (Pos(pos.pt, pos.dir.rot_ccw()), 1000),
        (Pos(pos.pt, pos.dir.rot_180()), 2000),
    ]
    moves_b = ((pos, sc) for pos, sc in moves_a
        if 0 <= pos.pt.x < len(lines[0]) and 0 <= pos.pt.y < len(lines)
    )
    moves = [(pos, sc) for pos, sc in moves_b
        if lines[pos.pt.y][pos.pt.x] in ["S",".", "E"]
    ]
    return moves

def walk_all(lines, st_pos):
    cheapest_costs = dict()
    cheapest_costs[st_pos] = 0
    cheapest_ways = dict()
    cheapest_ways[st_pos] = list()

    frontier = set([(st_pos, 0)])
    while len(frontier) > 0:
        #print(frontier)
        new_frontier = set()
        for front_pos, cur_cost in frontier:
            #print(front_pos, list_moves(lines, front_pos))
            for move, cost in list_moves(lines, front_pos):
                #if move.pt.x == 5 and move.pt.y == 7:
                #    print(move, front_pos, cost, cur_cost + cost)

                if move not in cheapest_costs or \
                    cheapest_costs[move] >= cost + cur_cost:
                    if move not in cheapest_costs or \
                        cheapest_costs[move] > cost + cur_cost:
                        cheapest_ways[move] = list()

                    cheapest_costs[move] = cost + cur_cost
                    cheapest_ways[move].append(front_pos)
                    new_frontier.add((move, cost + cur_cost))
        frontier = new_frontier

    return cheapest_costs, cheapest_ways

def get_paths(lines, cheapest_costs, cheapest_ways, ed_pt, st_pt):
    all_ed_pos = [Pos(ed_pt, dir) for dir in Direction]
    ed_pos_costs = [cheapest_costs[ed_pos] for ed_pos in all_ed_pos]
    cheapest = min(cheapest_costs[ed_pos] for ed_pos in all_ed_pos)
    cheapest_ends = [ed_pos
        for cost, ed_pos in zip(ed_pos_costs, all_ed_pos)
        if cost == cheapest]
    valid_ed_pos = [cheapest_ways[ed_pos]
        for ed_pos in cheapest_ends
        if ed_pos in cheapest_ways]
    
    on_path = set([ed_pt])
    frontier = set(chain.from_iterable(valid_ed_pos))
    on_path.update(way.pt for way in frontier)

    #print(frontier, on_path)
    while len(frontier) > 0:
        new_frontier = set()
        for front_pos in frontier:
            front_cost = cheapest_costs[front_pos]
            # all the cheapest prior positions to get here for front_cost
            ways = cheapest_ways[front_pos]
            #print("Ways", ways)

            new_frontier.update(ways)
            on_path.update(way.pt for way in ways)
                        
        #print("NEW ", new_frontier)
        frontier = new_frontier
    return on_path

def draw_map(lines, path):
    dm = [list(line) for line in lines]
    for pt in path:
        dm[pt.y][pt.x] = "O"
    return "\n".join("".join(line) for line in dm)

def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    st_pt_r = [(line.index("S"), row)
        for row, line in enumerate(lines) if "S" in line][0]
    ed_pt_r = [(line.index("E"), row)
        for row, line in enumerate(lines) if "E" in line][0]
    st_pt = Point(st_pt_r[0], st_pt_r[1])
    ed_pt = Point(ed_pt_r[0], ed_pt_r[1])

    cheapest_costs, cheapest_ways = walk_all(lines, Pos(st_pt, Direction.EAST))
    #print(cheapest_costs)
    all_ed_pos = [Pos(ed_pt, dir) for dir in Direction]
    part1_l = [cheapest_costs[ed_pos] for ed_pos in all_ed_pos if ed_pos in cheapest_costs]
    #print(part1_l)
    print(f"Part 1: {min(part1_l)}")
    
    path = get_paths(lines, cheapest_costs, cheapest_ways, ed_pt, st_pt)
    print(draw_map(lines, path))
    print(f"Part 2: {len(path)}")

if __name__ == "__main__":
    main(sys.argv[1])
