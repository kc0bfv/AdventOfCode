#!/usr/bin/env python3

import itertools as it

class Blizard:
    def __init__(self, height, width, initial, char):
        self.height = height
        self.width = width
        self.initial = initial
        self.char = char

        self.modval = self.height - 2 if char in "^v" else self.width - 2
        self.chng_ind = 1 if char in "^v" else 0
        self.dir = -1 if char in "^<" else 1

    def get_pos(self, roundno):
        changed = ((self.initial[self.chng_ind] - 1 + (roundno * self.dir)) % self.modval) + 1
        return (self.initial[0], changed) if self.chng_ind == 1 else (changed, self.initial[1])

class Blizards:
    def __init__(self, lines):
        self.height = len(lines)
        self.width = len(lines[0])
        self.blizards = list()

        self.blizard_rows = list(list() for _ in range(self.height))
        self.blizard_cols = list(list() for _ in range(self.width))

        for y, line in enumerate(lines):
            for x, char in enumerate(line):
                if char in ">^v<":
                    newbliz = Blizard(self.height, self.width, (x, y), char)
                    self.blizards.append(newbliz)
                    if char in "<>":
                        self.blizard_rows[y].append(newbliz)
                    elif char in "^v":
                        self.blizard_cols[x].append(newbliz)

    def check_pos(self, pos, curround):
        bliz_rows = self.blizard_rows[pos[1]]
        bliz_cols = self.blizard_cols[pos[0]]
        cur_bliz_pos = (bliz.get_pos(curround)
            for bliz in it.chain(bliz_rows, bliz_cols)
        )
        return not any(blizpos == pos for blizpos in cur_bliz_pos)

def do_bfs(blizards, start, goal, st_round = 0):
    moves = [(-1, 0), (1, 0), (0, -1), (0, 1), (0, 0)]

    frontier = set([start])
    
    curround = st_round
    while goal not in frontier and len(frontier) > 0:
        curround += 1
        new_frontier = set()
        for curf in frontier:
            raw_posmoves = ((curf[0] + pos[0], curf[1] + pos[1]) for pos in moves)
            nowall_posmoves = (move for move in raw_posmoves
                if move == start or move == goal or
                    (move[1] > 0 and move[0] > 0 and
                        move[1] < blizards.height-1 and move[0] < blizards.width-1
                    )
            )
            nobliz_posmoves = (move for move in nowall_posmoves
                if blizards.check_pos(move, curround))

            new_frontier.update(nobliz_posmoves)

        frontier = new_frontier
        print(f"Round: {curround} Size: {len(new_frontier)}")

    return curround

def main(filename):
    with open(filename, "r") as f:
        lines = [line.strip() for line in f]

    blizards = Blizards(lines)
    start = ([ind for ind, char in enumerate(lines[0]) if char == "."][0], 0)
    goal = ([ind for ind, char in enumerate(lines[-1]) if char == "."][0], len(lines) - 1)

    result = do_bfs(blizards, start, goal)

    print(f"Part 1: {result}")

    print("Getting Snacks")
    result_to_st = do_bfs(blizards, goal, start, result)
    print("Got Snacks")
    result_fin = do_bfs(blizards, start, goal, result_to_st)
    print("That Elf better be glad it's XMas")

    print(f"Part 2: {result_fin}")

if __name__ == "__main__":
    main("samp")
    main("samp2")
    main("input.txt")
