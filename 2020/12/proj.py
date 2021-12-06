#!/usr/bin/env python3

import math
from sys import argv

def vec_len(n, e):
    return math.sqrt((e ** 2) + (n ** 2))

def vec_deg(n, e):
    if e == 0:
        return 90 if n > 0 else 270
    elif n == 0:
        return 0 if e > 0 else 180
    else:
        ang = math.degrees(math.atan(abs(n/e)))
        if n > 0 and e > 0:
            return ang
        elif n > 0 and e < 0:
            return 180 - ang
        elif n < 0 and e < 0:
            return ang + 180
        elif n < 0 and e > 0:
            return 360 - ang
        else:
            print("ERROR angle {} {} {}".format(n, e, ang))

def vec_n(vecl, vecd):
    return round(vecl * math.sin(math.radians(vecd)))
def vec_e(vecl, vecd):
    return round(vecl * math.cos(math.radians(vecd)))

class Direc:
    def __init__(self, direc):
        self.cmd = direc[0]
        self.dist = int(direc[1:])
    def __str__(self):
        return "{} {}".format(self.cmd, self.dist)

class Ship:
    def __init__(self):
        self.n, self.e, self.o = 0, 0, 0
    def move(self, direc):
        if direc.cmd == "N":
            self.n += direc.dist
        elif direc.cmd == "S":
            self.n -= direc.dist
        elif direc.cmd == "E":
            self.e += direc.dist
        elif direc.cmd == "W":
            self.e -= direc.dist
        elif direc.cmd == "L":
            self.o += direc.dist
            self.o %= 360
        elif direc.cmd == "R":
            self.o -= direc.dist
            self.o %= 360
        elif direc.cmd == "F":
            self.n += direc.dist * math.sin(math.radians(self.o))
            self.e += direc.dist * math.cos(math.radians(self.o))

class Ship2:
    def __init__(self):
        self.n, self.e, self.wn, self.we = 0, 0, 1, 10
    def move(self, direc):
        if direc.cmd == "N":
            self.wn += direc.dist
        elif direc.cmd == "S":
            self.wn -= direc.dist
        elif direc.cmd == "E":
            self.we += direc.dist
        elif direc.cmd == "W":
            self.we -= direc.dist
        elif direc.cmd == "L":
            vec_l = vec_len(ship.wn, ship.we)
            vec_d = vec_deg(ship.wn, ship.we)
            vec_d += direc.dist
            vec_d %= 360
            self.wn = vec_n(vec_l, vec_d)
            self.we = vec_e(vec_l, vec_d)
        elif direc.cmd == "R":
            vec_l = vec_len(ship.wn, ship.we)
            vec_d = vec_deg(ship.wn, ship.we)
            vec_d -= direc.dist
            vec_d %= 360
            self.wn = vec_n(vec_l, vec_d)
            self.we = vec_e(vec_l, vec_d)
        elif direc.cmd == "F":
            self.n += direc.dist * self.wn
            self.e += direc.dist * self.we

if __name__ == "__main__":
    lines = (line.strip() for line in open(argv[1]).readlines())
    direcs = [Direc(line) for line in lines]
    ship = Ship()

    for direc in direcs:
        ship.move(direc)

    print("Part 1: {}".format(abs(round(ship.e)) + abs(round(ship.n))))

    ship = Ship2()
    for direc in direcs:
        ship.move(direc)
    print("Part 2: {}".format(abs(round(ship.e)) + abs(round(ship.n))))
