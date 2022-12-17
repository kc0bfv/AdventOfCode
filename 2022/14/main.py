#!/usr/bin/env python3

from collections import defaultdict
from enum import Enum

class Matter:
  SAND = "o"
  ROCK = "#"
  AIR = "."
  
class Cave:
  def __init__(self, inlines):
    self.map = defaultdict(lambda: Matter.AIR)
    self.min_x = 500
    self.min_y = 0
    self.max_x = 500
    self.max_y = 0
    for inline in inlines:
      strcmapoints = inline.split(" -> ")
      spoints = [scpoint.split(",") for scpoint in strcmapoints]
      points = [(int(spointx), int(spointy)) for spointx, spointy in spoints]
      segs = zip(points, points[1:])
      for st_pt, ed_pt in segs:
        print(st_pt, ed_pt)
        chng_axis = 1 if st_pt[0] == ed_pt[0] else 0
        move_dist = ed_pt[chng_axis] - st_pt[chng_axis]
        assert(move_dist != 0)
        move_dir = 1 if move_dist > 0 else -1
        for ind in range(0, move_dist + move_dir, move_dir):
          cur_pt_x = st_pt[0] + ind if chng_axis == 0 else st_pt[0]
          cur_pt_y = st_pt[1] if chng_axis == 0 else st_pt[1] + ind
          self.update_map((cur_pt_x, cur_pt_y), Matter.ROCK)

    self.floor = self.max_y + 2

  def update_map(self, point, matter):
    self.map[point] = matter
    if point[0] < self.min_x:
      self.min_x = point[0]
    if point[0] > self.max_x:
      self.max_x = point[0]
    if point[1] < self.min_y:
      self.min_y = point[1]
    if point[1] > self.max_y:
      self.max_y = point[1]
  
  def __str__(self):
    return "\n".join(
      "".join(
        self.map[(x, y)] for x in range(self.min_x, self.max_x + 1)
      ) for y in range(self.min_y, self.max_y + 1)
    )

  def drop_sand(self, no_bottom=True):
    cur_pos = (500,0)
    if self.map[cur_pos] != Matter.AIR:
      # No more space for sand
      return None
    bottom = self.max_y if no_bottom else self.floor
    while cur_pos[1] <= bottom:
      try_pos = [
        (cur_pos[0], cur_pos[1] + 1),
        (cur_pos[0] - 1, cur_pos[1] + 1),
        (cur_pos[0] + 1, cur_pos[1] + 1)
      ]
      next_pos = [pos for pos in try_pos if pos[1] < self.floor and self.map[pos] == Matter.AIR]
      if len(next_pos) == 0:
        self.update_map(cur_pos, Matter.SAND)
        return cur_pos
      cur_pos = next_pos[0]

      #TODO handle infinite floor
    # Sand fell out the bottom
    if no_bottom:
      return None
    raise RuntimeError("Shouldn't be here")
      
def main(filename):
  with open(filename, "r") as f:
    lines = [l.strip() for l in f]

  cave = Cave(lines)
  #print(cave)

  round = 0
  result = "not nothing"
  while result is not None:
    round += 1
    result = cave.drop_sand()
    #print(round)
    #print(cave)

  print(f"Part 1: {round-1}")

  result = "not nothing again"
  while result is not None:
    round += 1
    result = cave.drop_sand(False)
    #print(round)
    #print(cave)

  print(f"Part 2: {round-2}")

if __name__ == "__main__":
  main("samp")
  main("input.txt")