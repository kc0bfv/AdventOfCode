#!/usr/bin/env python3

from collections import deque

class Point(tuple):
    def getNext(self, dirs):
        if dirs == "U":
            return Point((self[0], self[1]-1))
        elif dirs == "D":
            return Point((self[0], self[1]+1))
        elif dirs == "L":
            return Point((self[0]-1, self[1]))
        elif dirs == "R":
            return Point((self[0]+1, self[1]))
        else:
            raise RuntimeError("Invalid getNext")

class Line:
    def __init__(self, start_pt, dirs, leni):
        self.start_pt = start_pt
        end_pt = None
        if dirs == "U":
            end_pt = (start_pt[0], start_pt[1] - leni)
            self.cons = 0
        elif dirs == "D":
            end_pt = (start_pt[0], start_pt[1] + leni)
            self.cons = 0
        elif dirs == "L":
            end_pt = (start_pt[0] - leni, start_pt[1])
            self.cons = 1
        elif dirs == "R":
            end_pt = (start_pt[0] + leni, start_pt[1])
            self.cons = 1
        self.non_cons = 1 if self.cons == 0 else 0
        self.min_pt, self.max_pt = (start_pt, end_pt) if start_pt[self.non_cons] < end_pt[self.non_cons] else (end_pt, start_pt)
    
    def isIn(self, pt):
        if pt[self.cons] != self.min_pt[self.cons]:
            return False
        return self.min_pt[self.non_cons] <= pt[self.non_cons] <= self.max_pt[self.non_cons]

class DigMap:
    def __init__(self):
        self.wall_pts = set() # of Point
        self.lines = list()
        self.min_x, self.max_x = -1, 1
        self.min_y, self.max_y = -1, 1

    def addLine(self, cur_pt_in, dirs, len):
        new_line = Line(cur_pt_in, dirs, len)
        self.lines.append(new_line)
        if new_line.min_pt[0] - 1 < self.min_x:
            self.min_x = new_line.min_pt[0] - 1
        if new_line.min_pt[1] - 1 < self.min_y:
            self.min_y = new_line.min_pt[1] - 1
        if new_line.max_pt[0] + 1 > self.max_x:
            self.max_x = new_line.max_pt[0] + 1
        if new_line.max_pt[1] + 1 > self.max_y:
            self.max_y = new_line.max_pt[1] + 1
        return new_line.min_pt if new_line.min_pt != cur_pt_in else new_line.max_pt

        # Returns ending point
        print(f"f {len}")
        cur_pt = cur_pt_in
        for _ in range(len):
            cur_pt = cur_pt.getNext(dirs)
            self.wall_pts.add(cur_pt)
        return cur_pt
        
    def onWall(self, pt):
        return any(line.isIn(pt) for line in self.lines)

    def calcInside(self):
        pass
        total = 0
        for ind, _ in enumerate(self.lines):
            uno = self.lines[ind]
            dos = self.lines[(ind + 1) % len(self.lines)]
            total += (uno.start_pt[0] * dos.start_pt[1]) - (uno.start_pt[1] * dos.start_pt[0])
        return total/2

    def calcInsideOld(self):
        min_x = self.min_x
        max_x = self.max_x
        min_y = self.min_y
        max_y = self.max_y
        print(min_x, max_x, min_y, max_y)
        
        frontier = set([(min_x, min_y)])
        explored = set()
        exterior = set([(min_x, min_y)])
        while len(frontier) > 0:
            cur = frontier.pop()
            explored.add(cur)
            expand = [(-1, 0), (1, 0), (0, -1), (0, 1)]
            prospective = ((cur[0] + x, cur[1] + y) for (x, y) in expand)
            filt1 = (Point((x, y)) for (x, y) in prospective if min_x<=x<=max_x and min_y<=y<=max_y)
            filt2 = [pt for pt in filt1 if not self.onWall(pt) and pt not in explored]
            frontier.update(filt2)
            exterior.update(filt2)
        
        return ((1 + max_x - min_x) * (1 + max_y - min_y)) - len(exterior)

    def __str__(self):
        min_x = min(pt[0] for pt in self.wall_pts) - 1
        max_x = max(pt[0] for pt in self.wall_pts) + 1
        min_y = min(pt[1] for pt in self.wall_pts) - 1
        max_y = max(pt[1] for pt in self.wall_pts) + 1
        return "\n".join("".join("#" if (x,y) in self.wall_pts else "." for x in range(min_x, max_x+1)) for y in range(min_y, max_y+1))
    

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    
    split_lines = [line.split(" ") for line in lines]

    digmap = DigMap()
    cur = Point((0, 0))
    for (dirs, lens, _) in split_lines:
        cur = digmap.addLine(cur, dirs, int(lens))

    #print(digmap)
    inside_size = digmap.calcInside()
    print(f"Part 1: {inside_size}")
    
    dir_lookup = ["R", "D", "L", "U"]
    p2lines = [(dir_lookup[int(color[7])], int(color[2:7],16)) for (_,_,color) in split_lines]
    
    return
    digmap2 = DigMap()
    cur = Point((0, 0))
    for (dirs, leni) in p2lines:
        cur = digmap2.addLine(cur, dirs, leni)
    inside_size = digmap2.calcInside()
    print(f"Part 2: {inside_size}")

if __name__ == "__main__":
    main("samp02")
    #main("samp01")
    #main("input")