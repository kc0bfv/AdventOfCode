#!/usr/bin/env python3

class MapBuilder:
    def __init__(self, line):
        # Dict with (x, y) as key, shortest path to a room as value
        self.room_dict = dict()

        self.build_dict(line, 0, 0, 0)

    def build_dict(self, dat, initcurx, initcury, initdoors):  
        dir_dict = {"N": (0, 1), "S": (0, -1), "E": (1, 0), "W": (-1, 0)}
        curx, cury, doors = initcurx, initcury, initdoors
        i = 0
        while i < len(dat):
            #print(i, dat, self.room_dict)
            if dat[i] in dir_dict:
                doors += 1
                curx, cury = curx + dir_dict[dat[i]][0], cury + dir_dict[dat[i]][1]
                if (curx, cury) not in self.room_dict or \
                        self.room_dict[(curx, cury)] > doors:
                    self.room_dict[(curx, cury)] = doors
            elif dat[i] == "(":
                chars_proced = self.build_dict(dat[i+1:], curx, cury, doors)
                i += chars_proced + 1
            elif dat[i] == ")":
                return i
            elif dat[i] == "|":
                return i + self.build_dict(dat[i+1:], initcurx, initcury, initdoors) + 1

            i += 1


if __name__ == "__main__":
    with open("input day 20.txt") as f:
        line = next(f)

    if line[0] != "^" and line[-1] != "$":
        raise RuntimeError("Bad {} {}".format(line[0], line[-1]))

    mapdat = MapBuilder(line[1:-1])

    print(mapdat.room_dict)
    print(max(mapdat.room_dict.values()))

    over1k = [i for i in mapdat.room_dict.values() if i >= 1000]
    print(len(over1k))
