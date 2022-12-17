#!/usr/bin/env python3

class Board:
  def __init__(self, lines):
    self.map = [list(line) for line in lines]
    self.start = None
    self.end = None
    for y in range(len(self.map)):
      for x in range(len(self.map[0])):
        if self.map[y][x] == "S":
          self.start = (x, y)
          self.map[y][x] = "a"
        elif self.map[y][x] == "E":
          self.end = (x, y)
          self.map[y][x] = "z"
    assert(self.start != None and self.end != None)
    
  def on_board(self, pos):
    return not (pos[0] < 0 or pos[1] < 0 or pos[0] >= len(self.map[0]) or pos[1] >= len(self.map))
    
  def get_elev(self, pos):
    return ord(self.map[pos[1]][pos[0]])-ord("a")

def expand_all(board, pos, no_go1, no_go2):
  dirs = [(-1,0),(1,0),(0,-1),(0,1)]
  expanded = ((pos[0]+dir[0], pos[1]+dir[1]) for dir in dirs)
  return [new_pos for new_pos in expanded
          if board.on_board(new_pos)
          and new_pos not in no_go1
          and new_pos not in no_go2
         ]

def expand_test_p1(board, new_pos, cur_pos):
  return board.get_elev(new_pos) <= board.get_elev(cur_pos) + 1
  
def expand_test_p2(board, new_pos, cur_pos):
  return board.get_elev(new_pos) >= board.get_elev(cur_pos) - 1

def end_in_frontier(board, frontier):
  return board.end in frontier

def has_any_pt_a(board, pts):
  return 0 in (board.get_elev(pt) for pt in pts)
  
def do_bfs(board, start_pt, end_test, expand_test):
  frontier = set([start_pt])
  past_pos = set()
  steps = 0
  while not end_test(board, frontier) and len(frontier) > 0:
    steps += 1
    new_area = set()
    for pos in frontier:
      new_area.update(new_pos for new_pos in expand_all(board, pos, past_pos, frontier)
                      if expand_test(board, new_pos, pos)
                     )
    past_pos.update(frontier)
    frontier = new_area
  if len(frontier) == 0:
    raise RuntimeError("Out of frontier")
  return steps

    
def main(filename):
  with open(filename, "r") as f:
    lines = [line.strip() for line in f]

  board = Board(lines)
  steps = do_bfs(board, board.start, end_in_frontier, expand_test_p1)
  print(f"Part 1: {steps}")
  steps = do_bfs(board, board.end, has_any_pt_a, expand_test_p2)
  print(f"Part 2: {steps}")
  
if __name__ == "__main__":
  main("samp")
  main("input.txt")