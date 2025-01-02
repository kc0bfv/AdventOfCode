#!/usr/bin/env python3

from collections import defaultdict, namedtuple
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
    deltas = [
        (Pos(pos.dir.fwd_delta(), pos.dir), 1),
        (Pos(pos.dir.rot_cw().fwd_delta(), pos.dir.rot_cw()), 1001),
        (Pos(pos.dir.rot_ccw().fwd_delta(), pos.dir.rot_ccw()), 1001),
        (Pos(pos.dir.rot_180().fwd_delta(), pos.dir.rot_180()), 2001),
    ]
    moves_a = ((Pos(add_pts(pos.pt, delt.pt), delt.dir), sc) for delt, sc in deltas)
    moves_b = ((pos, sc) for pos, sc in moves_a
        if 0 <= pos.pt.x < len(lines[0]) and 0 <= pos.pt.y < len(lines)
    )
    moves = [(pos, sc) for pos, sc in moves_b
        if lines[pos.pt.y][pos.pt.x] in [".", "E"]
    ]
    return moves

def list_moves_backward(lines, pos):
    def fix_cost(cost):
        return {1: 2001, 2001: 1, 1001: 1001}[cost]
    return [(Pos(pos.pt, pos.dir.rot_180()), fix_cost(cost))
        for pos, cost in list_moves(lines, pos)]

def walk_all(lines, st_pos):
    cheapest_costs = dict()
    cheapest_costs[st_pos.pt] = 0
    cheapest_ways = dict()
    cheapest_ways[st_pos.pt] = list([st_pos])

    frontier = set([(st_pos, 0)])
    while len(frontier) > 0:
        new_frontier = set()
        for front_pos, cur_cost in frontier:
            for move, cost in list_moves(lines, front_pos):
                if move.pt.x == 5 and move.pt.y == 7:
                    print(move, front_pos, cost, cur_cost + cost)
                if move.pt not in cheapest_costs or \
                    cheapest_costs[move.pt] >= cost + cur_cost:
                    if move.pt not in cheapest_costs or \
                        cheapest_costs[move.pt] > cost + cur_cost:
                        cheapest_ways[move.pt] = list()

                    cheapest_costs[move.pt] = cost + cur_cost
                    cheapest_ways[move.pt].append(move)
                    new_frontier.add((move, cost + cur_cost))
        frontier = new_frontier

    return cheapest_costs, cheapest_ways

def get_paths(lines, cheapest_costs, cheapest_ways, ed_pt, st_pt):
    on_path = set()

    frontier = set(cheapest_ways[ed_pt])
    on_path.update(way.pt for way in frontier)
    print(frontier, on_path)
    while len(frontier) > 0:
        new_frontier = set()
        for front_pos in frontier:
            front_cost = cheapest_costs[front_pos.pt]

            # Check each of the nearby positions we could have come from
            poss_delts = [(1,0), (-1, 0), (0, 1), (0, -1)]
            for delt in poss_delts:
                nearby_pt = add_pts(front_pos.pt, Point(*delt))
                if nearby_pt not in cheapest_costs:
                    continue
                if cheapest_costs[nearby_pt] >= front_cost:
                    continue

                # Get all of the cheapest ways to get to the nearby_pt
                nearby_pt_ways = cheapest_ways[nearby_pt]
                #print(nearby_pt, nearby_pt_ways)
                
                # For each of those nearby points look at possible moves
                # and see if our frontier is one of them
                for nearby_pt_way in nearby_pt_ways:
                    moves = list_moves(lines, nearby_pt_way)
                    for move, cost in moves:
                        if move.pt == front_pos.pt and \
                            move.dir == front_pos.dir and \
                            cheapest_costs[nearby_pt] + cost == front_cost:
                            on_path.add(nearby_pt)
                            new_frontier.add(nearby_pt_way)
                        
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
    part1 = cheapest_costs[ed_pt]
    print(f"Part 1: {part1}")

    path = get_paths(lines, cheapest_costs, cheapest_ways, ed_pt, st_pt)
    print(draw_map(lines, path))
    print(f"Part 2: {len(path)}")

if __name__ == "__main__":
    main(sys.argv[1])
