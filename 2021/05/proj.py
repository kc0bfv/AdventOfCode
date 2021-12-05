#!/usr/bin/env python3

from sys import argv

class Point:
    def __init__(self, desc):
        x, y = desc.split(",")
        self.x, self.y = int(x), int(y)
    def __str__(self):
        return "{},{}".format(self.x, self.y)

class Line:
    def __init__(self, desc):
        uno, dos = desc.split(" -> ")
        self.uno, self.dos = Point(uno), Point(dos)
    def is_horiz(self):
        return self.uno.y == self.dos.y
    def is_vert(self):
        return self.uno.x == self.dos.x
    def walk(self):
        x_diff = self.dos.x - self.uno.x
        y_diff = self.dos.y - self.uno.y
        if x_diff == 0:
            x_step, y_step = 0, 1 if y_diff > 0 else -1
        elif y_diff == 0:
            x_step, y_step = 1 if x_diff > 0 else -1, 0
        elif x_diff == y_diff or x_diff == -y_diff:
            x_step, y_step = 1 if x_diff > 0 else -1, 1 if y_diff > 0 else -1
        else:
            print("ERROR: {} {}".format(x_diff, y_diff))
        x_pos, y_pos = self.uno.x, self.uno.y
        yield x_pos, y_pos
        while x_pos != self.dos.x or y_pos != self.dos.y:
            x_pos += x_step
            y_pos += y_step
            yield x_pos, y_pos
    def __str__(self):
        return "{} -> {}".format(self.uno, self.dos)

class Grid:
    def __init__(self, xmax, ymax):
        self.grid = [[0 for _ in range(xmax+1)] for _ in range(ymax+1)]
    def __str__(self):
        return "\n".join(" ".join(str(col) for col in row) for row in self.grid)
    def add_line(self, line):
        for x,y in line.walk():
            self.grid[y][x] += 1
    def count_overlaps_over(self, min_o):
        vals = ([i for i in row if i > min_o] for row in self.grid)
        return sum(len(row) for row in vals)

if __name__ == "__main__":
    lines = [Line(line.strip()) for line in open(argv[1]).readlines()]
    all_y1s = (line.uno.y for line in lines)
    all_y2s = (line.dos.y for line in lines)
    all_x1s = (line.uno.x for line in lines)
    all_x2s = (line.dos.x for line in lines)
    max_y = max(max(all_y1s), max(all_y2s))
    max_x = max(max(all_x1s), max(all_x2s))
    grid = Grid(max_x, max_y)
    
    hv_lines = [line for line in lines if (line.is_horiz() or line.is_vert())]

    for line in hv_lines:
        grid.add_line(line)

    print("Part 1: {}".format(grid.count_overlaps_over(1)))

    grid = Grid(max_x, max_y)
    for line in lines:
        grid.add_line(line)
    print("Part 2: {}".format(grid.count_overlaps_over(1)))
