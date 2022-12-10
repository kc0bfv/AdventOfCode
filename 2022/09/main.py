#!/usr/bin/env python3

class Rope:
  def __init__(self, num_knots):
    self.knots = [(0,0) for _ in range(num_knots)]
    self.knots_pos = list(list() for _ in range(num_knots))

  def move(self, dir, dist):
    dir_l = { "R": (1, 0), "L": (-1, 0), "U": (0, 1), "D": (0, -1) }
    dir_t = dir_l[dir]
    for _ in range(dist):
      self.knots[0] = (self.knots[0][0] + dir_t[0], self.knots[0][1] + dir_t[1])
      #print(dir, 1, self.knots[0])
      for ind, _ in enumerate(self.knots):
        if ind > 0:
          self.update_tail(ind)
        # knot has been updated, so can't use the enumerated one above
        self.knots_pos[ind].append(self.knots[ind])

  def update_tail(self, ind):
    lead, tail = self.knots[ind-1], self.knots[ind]
    pos0_dist = lead[0] - tail[0]
    pos1_dist = lead[1] - tail[1]
    
    if abs(pos0_dist) <= 1 and abs(pos1_dist) <= 1:
      new0, new1 = tail
    elif abs(pos0_dist) <= 2 and abs(pos1_dist) <= 2:
      direc0 = -1 if pos0_dist > 0 else 1
      direc1 = -1 if pos1_dist > 0 else 1
      new0, new1 = lead
      if abs(pos0_dist) == 2:
        new0 = tail[0] + pos0_dist + direc0
      if abs(pos1_dist) == 2:
        new1 = tail[1] + pos1_dist + direc1
    else:
      raise RuntimeError(f"Invalid pos dist: {lead} {tail}")

    self.knots[ind] = (new0, new1)

def main(filename):
  with open(filename, "r") as f:
    lines = [l.strip() for l in f]

  rope = Rope(2)
  rope2 = Rope(10)

  for line in lines:
    dir, dist = line.split(" ")
    rope.move(dir, int(dist))
    rope2.move(dir, int(dist))

  tail_pos = set(rope.knots_pos[-1])
  tail2_pos = set(rope2.knots_pos[-1])
  
  print(f"Part 1: {len(tail_pos)}")
  print(f"Part 2: {len(tail2_pos)}")

if __name__ == "__main__":
  main("samp2")
  main("input.txt")