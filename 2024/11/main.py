#!/usr/bin/env python3

from collections import defaultdict
import itertools
import sys

# Part 1

def blink(val):
    if val == 0:
        return (1,)
    sval = str(val)
    if len(sval) % 2 == 0:
        uno = int(sval[0:int(len(sval)/2)])
        dos = int(sval[int(len(sval)/2):])
        return (uno, dos)
    return (val * 2024,)


def blink_line(vals):
    return list(itertools.chain.from_iterable((blink(val) for val in vals)))

# Part 2 total change...
class BO:
    def __init__(self, val, boreg=None):
        self.val = val
        self.blinks = 0
        self.components = None
        self.boreg = dict() if boreg is None else boreg

        # If this val is already seen, we're only a reference
        self.isref = val in self.boreg
        # If we're not a ref, we're orig, so mark it seen
        if not self.isref:
            self.boreg[val] = dict()

    def blink(self):
        # Return the number of components for this val at the next blink level
        self.blinks += 1
        if self.isref:
            return self.boreg[self.val][self.blinks]

        if self.blinks == 1:
            # Expand this one into self.components
            self.components = tuple(BO(v, self.boreg) for v in blink(self.val))
            # Return the expansion size
            self.boreg[self.val][self.blinks] = len(self.components)
        else:
            # Blink each component
            self.boreg[self.val][self.blinks] = sum(c.blink() for c in self.components)

        return self.boreg[self.val][self.blinks]

def main(filename):
    with open(filename) as infile:
        lines = (line for line in infile)
        line = [int(val) for val in next(lines).strip().split()]

    # Part 1
    """
    curln = list(line)
    #print(f"Orig {len(curln)}: {curln}")
    for blnk in range(25 if cnt > 25 else cnt):
        curln = blink_line(curln)
        #print(f"Blink {blnk} {len(curln)}: {curln}")
    print(f"Part 1: {len(curln)}")
    """
    p1cnt = 25
    curln = list(BO(v) for v in line)
    blinkvs = {0: len(curln)}
    for blnk in range(p1cnt):
        blinkvs[blnk+1] = sum(c.blink() for c in curln)
    print(f"Part 1: {blinkvs[p1cnt]}")

    # Part 2
    cnt = 75
    curln = list(BO(v) for v in line)
    blinkvs = {0: len(curln)}
    for blnk in range(cnt):
        blinkvs[blnk+1] = sum(c.blink() for c in curln)
    print(f"Part 2: {blinkvs[cnt]}")

if __name__ == "__main__":
    main(sys.argv[1])
