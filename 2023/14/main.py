#!/usr/bin/env python3

class Plat:
    def __init__(self, lines):
        self.grid = [list(line) for line in lines]
    def roll(self, dir):
        # X offset, Y offset, rev x, rev y
        dirlook = {
            "north": (0, -1, False, False),
            "south": (0, 1, False, True), 
            "west": (-1, 0, False, False),
            "east": (1, 0, True, False),
        }
        xoff, yoff, revx, revy = dirlook[dir]
        y_enum = list(range(len(self.grid)))
        if revy:
            y_enum = list(reversed(y_enum))
        x_enum = list(range(len(self.grid[0])))
        if revx:
            x_enum = list(reversed(x_enum))
        for y in y_enum:
            for x in x_enum:
                if self.grid[y][x] == "O":
                    tempy, tempx = y, x
                    while 0 <= (tempy+yoff) < len(self.grid) and 0 <= (tempx+xoff) < len(self.grid[0]) and self.grid[tempy+yoff][tempx+xoff] == ".":
                        tempy += yoff
                        tempx += xoff
                    self.grid[y][x] = "."
                    self.grid[tempy][tempx] = "O"
    def score(self):
        score = 0
        for y, line in enumerate(self.grid):
            score += line.count("O") * (len(self.grid) - y)
        return score
    def __str__(self):
        return "\n".join("".join(line) for line in self.grid)

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]

    p = Plat(lines)
    p.roll("north")
    print(f"Part 1: {p.score()}")

    p = Plat(lines)
    scores = list()
    amt = 500
    for i in range(amt):
        #print(i)
        p.roll("north")
        #print(p)
        #print("")
        p.roll("west")
        #print(p)
        #print("")
        p.roll("south")
        #print(p)
        #print("")
        p.roll("east")
        #print(p)
        #print("")
        scores.append(p.score())
    print(scores)
    scores.reverse()
    rep_pt = scores.index(scores[0], 1)
    scores.reverse()
    part_2_ind = (1000000000 - amt) % rep_pt
    print(rep_pt, part_2_ind)
    part_2 = scores[(len(scores) - rep_pt - 1) + part_2_ind]
    print(f"Part 2: {part_2}")

if __name__ == "__main__":
    main("samp01")
    main("input")