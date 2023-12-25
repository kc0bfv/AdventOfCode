#!/usr/bin/env python3

import re

class Step:
    def __init__(self, stepstr):
        patt = "([a-zA-Z0-9]+)([-=])([0-9]*)"
        self.label, self.op, flen = re.match(patt, stepstr).groups()
        self.flen = int(flen) if self.op == "=" else None
        self.box = custhash(self.label)

class Box:
    def __init__(self, num):
        self.num = num
        self.lenses = dict() # key - label, val - (index, flen)
        self.index = 0
    def remove(self, label):
        if label in self.lenses:
            del self.lenses[label]
    def add(self, label, flen):
        if label in self.lenses:
            old = self.lenses[label]
            self.lenses[label] = (old[0], flen)
        else:
            self.lenses[label] = (self.index, flen)
            self.index += 1
    def get_power(self):
        lense_sorted = sorted(self.lenses.values())
        vals = [(self.num + 1) * (ind + 1) * flen for ind, (_, flen) in enumerate(lense_sorted)]
        return sum(vals)

class Boxes:
    def __init__(self):
        self.boxes = list(Box(i) for i in range(256))
    def runstep(self, step):
        if step.op == "-":
            self.boxes[step.box].remove(step.label)
        elif step.op == "=":
            self.boxes[step.box].add(step.label, step.flen)
        else:
            raise RuntimeError("runstep err")
    def get_power(self):
        return sum(box.get_power() for box in self.boxes)

def custhash(seg):
    cur_val = 0
    for char in seg:
        cur_val += ord(char)
        cur_val *= 17
        cur_val %= 256
    return cur_val

def main(filename):
    with open(filename) as f:
        line = f.read().strip()

    segs = line.split(",")
    hashed = [custhash(seg) for seg in segs]
    print(f"Part 1: {sum(hashed)}")

    p2steps = [Step(seg) for seg in segs]
    boxes = Boxes()
    _ = [boxes.runstep(step) for step in p2steps]
    p2power = boxes.get_power()
    print(f"Part 2: {p2power}")

if __name__ == "__main__":
    main("samp01")
    main("input")