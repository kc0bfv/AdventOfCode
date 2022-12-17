#!/usr/bin/env python3

class FlexiRange:
  def __init__(self, iter):
    self.rng_lst = list(iter)
    self.do_combine()
    
  def do_combine(self):
    self.rng_lst.sort()
    new_lst = list()
    cur_st, cur_ed = self.rng_lst[0]
    for rng_st, rng_ed in self.rng_lst[1:]:
      if rng_st <= cur_ed + 1:
        if rng_ed > cur_ed:
          cur_ed = rng_ed
        else:
          #do nothing, fully contained
          pass
      else:
        new_lst.append((cur_st, cur_ed))
        cur_st, cur_ed = rng_st, rng_ed

    new_lst.append((cur_st, cur_ed))
    self.rng_lst = new_lst
  
  def add_in(self, new):
    self.rng_lst.append(new)
    self.do_combine()

class SenseRet:
  def __init__(self, line):
    lsplit = line.split(" ")
    strsx, strsy, strcx, strcy = lsplit[2][2:-1], lsplit[3][2:-1], lsplit[8][2:-1], lsplit[9][2:]
    self.sensor = (int(strsx), int(strsy))
    self.beacon = (int(strcx), int(strcy))
    self.dist = abs(self.sensor[0] - self.beacon[0]) + abs(self.sensor[1] - self.beacon[1])

  def get_occupied(self, row):
    amt = self.dist - abs(row - self.sensor[1])
    if amt < 0:
      return None
    return (self.sensor[0]-amt, self.sensor[0]+amt)

def check_row(senserets, test_row):
  all_occupied = (s.get_occupied(test_row) for s in senserets)
  return FlexiRange(o for o in all_occupied if o is not None)
    
def main(filename, test_row, min_row, max_row):
  with open(filename, "r") as f:
    senserets = [SenseRet(l.strip()) for l in f]

  occd = check_row(senserets, test_row)
  beacons = {s.beacon for s in senserets if s.beacon[1] == test_row}
  
  p1 = (occd.rng_lst[0][1]+1 - occd.rng_lst[0][0]) - len(beacons)
    
  print(f"Part 1: {p1}")

  for row in range(min_row, max_row):
    occd = check_row(senserets, row)
    if len(occd.rng_lst) > 1:
      freq = row + 4000000 * (occd.rng_lst[0][1]+1)
      print(f"Part 2: {freq}")

if __name__ == "__main__":
  main("samp", 10, 0, 20)
  main("input.txt", 2000000, 0, 4000000)