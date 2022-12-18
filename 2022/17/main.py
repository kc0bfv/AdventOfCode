#!/usr/bin/env python3

import itertools as it

ROCKS = [
    ["####"],
    [" # ",
     "###",
     " # "],
    ["  #",
     "  #",
     "###"],
    ["#","#","#","#"],
    ["##",
     "##"],
]

class Rock:
    def __init__(self, rock, highest_init):
        self.rock_pts = set()
        for topcol, line in enumerate(rock):
            col = len(rock) - 1 - topcol
            for row, char in enumerate(line):
                if char == "#":
                    self.rock_pts.add((row + 2, col + highest_init + 4))

    def push(self, screen_pts, jet):
        if jet == ">":
            #print("Push right")
            self.try_move(screen_pts, (1, 0))
        elif jet == "<":
            #print("Push left")
            self.try_move(screen_pts, (-1, 0))
        else:
            raise RuntimeError("Invalid jet")

    def drop(self, screen):
        retval = self.try_move(screen, (0, -1))
        if retval:
            #print("Dropped")
            pass
        else:
            #print("Stopped")
            pass
        return retval

    def try_move(self, screen, offset):
        new_pts = set()
        for pt in self.rock_pts:
            new_pt = pt[0] + offset[0], pt[1] + offset[1]
            if new_pt[0] < 0 or new_pt[0] >= 7:
                return False
            if new_pt[1] < 0:
                return False
            if new_pt in screen.screen_pts:
                return False
            new_pts.add(new_pt)
        self.rock_pts = new_pts
        return True

class Screen:
    def __init__(self):
        self.screen_pts = set()
        self.max_height = -1

    def add_rock(self, rock):
        self.screen_pts.update(rock.rock_pts)
        highest_rock_pt = max(pt[1] for pt in rock.rock_pts)
        self.max_height = max(self.max_height, highest_rock_pt)

    def special_str(self, rock=None):
        screen = ""
        for col in range(self.max_height + 8, 0-1, -1):
            screen += "|"
            for row in range(7):
                if (row, col) in self.screen_pts:
                    screen += "#"
                elif rock is not None and (row, col) in rock.rock_pts:
                    screen += "@"
                else:
                    screen += " "

            screen += "|\n"
        screen += "+-------+"
        return screen

    def __str__(self):
        return self.special_str()

def do_step(screen, rock, jet):
    rock.push(screen, jet)
    dropped = rock.drop(screen)
    if not dropped:
        screen.add_rock(rock)
        return True
    return False

def main(filename, warmup, period):
    with open(filename, "r") as f:
        dirs = list(f.read().strip())

    screen = Screen()

    infinite_rock_consts = it.cycle(rock for rock in ROCKS)
    infinite_rocks = (Rock(rock, screen.max_height) for rock in infinite_rock_consts)
    infinite_dirs = it.cycle(dirs)

    rocks_set = 0
    cur_rock = next(infinite_rocks)
    height_changes = list()
    rock_cnt = 2022
    while rocks_set < rock_cnt:
        pre_height = screen.max_height
        rock_stopped = do_step(screen, cur_rock, next(infinite_dirs))
        #print()
        #print(screen.special_str(cur_rock))
        if rock_stopped:
            height_changes.append(screen.max_height - pre_height)
            rocks_set += 1
            cur_rock = next(infinite_rocks)

    #print()
    #print(screen)

    print(f"Part 1: {screen.max_height + 1}")
    #print("".join(str(i) for i in height_changes))

    rock_cnt = 1000000000000
    first = sum(height_changes[:warmup])
    multer = sum(height_changes[warmup:(warmup+period)])
    mult = multer * int((rock_cnt - warmup) / period)
    remcnt = (rock_cnt - warmup) % period
    rem = sum(height_changes[warmup:warmup+remcnt])
    print(f"Part 2: {first+mult+rem}")

if __name__ == "__main__":
    #main("samp", 16, 35)
    main("input.txt", 200, 1690)
