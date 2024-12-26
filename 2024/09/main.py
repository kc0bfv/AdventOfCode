#!/usr/bin/env python3

from collections import defaultdict
import sys

class FSBlock:
    def __init__(self, idval, initloc, length):
        self.idval = idval
        self.locs = list()
        self.movedloc = list()
        for loc in range(initloc, initloc + length):
            self.locs.append(loc)
    def all_moved(self):
        return len(self.locs) == 0
    def can_move(self, freeloc):
        if freeloc is None:
            raise RuntimeError("Invalid freeloc")
        if self.all_moved():
            raise RuntimeError("All moved!")
        return self.locs[-1] > freeloc
    def move_one(self, freeloc):
        if freeloc is None:
            raise RuntimeError("Invalid freeloc")
        if self.all_moved():
            raise RuntimeError("All moved!")
        self.movedloc.append(freeloc)
        del self.locs[-1]
    def move_all(self, freeloc):
        curloc = freeloc
        while len(self.locs) > 0:
            self.move_one(curloc)
            curloc += 1
        
    def checksum(self):
        return sum(loc * self.idval for loc in self.locs) + \
            sum(loc * self.idval for loc in self.movedloc)
    def __str__(self):
        return f"({self.idval}: " + \
            ",".join(str(v) for v in self.movedloc) + \
            "..." + ",".join(str(v) for v in self.locs)

class FreeBlock:
    def __init__(self, initloc, length):
        self.initloc = initloc
        self.length = length
    def take_some(self, cnt):
        if self.length < cnt:
            return None
        self.length -= cnt
        start_pt = self.initloc
        self.initloc += cnt
        return start_pt
    def peek_one(self):
        if self.length <= 0:
            return None
        else:
            return self.initloc

def part1(inline):
    datas = list()
    frees = list()
    idval = 0
    curloc = 0
    onval = True
    for val in inline:
        if onval:
            datas.append(FSBlock(idval, curloc, val))
            idval += 1
        else:
            frees.append(FreeBlock(curloc, val))
        curloc += val
        onval = not onval

    freeind = 0
    dataind = len(datas)-1
    while True:
        curfree = frees[freeind]
        # If there are no frees left in current block, go to next
        if curfree.peek_one() is None:
            freeind += 1
            continue

        curdata = datas[dataind]
        # If all data blocks in current block have been moved, go to next
        if curdata.all_moved():
            dataind -= 1
            continue

        # If not all blocks are moved but none can move, because
        # the free block is after the data block, then we're done
        if not curdata.can_move(curfree.peek_one()):
            break

        curdata.move_one(curfree.take_some(1))

    return sum(block.checksum() for block in datas)

def part2(inline):
    datas = list()
    frees = list()
    idval = 0
    curloc = 0
    onval = True
    for val in inline:
        if onval:
            datas.append(FSBlock(idval, curloc, val))
            idval += 1
        else:
            frees.append(FreeBlock(curloc, val))
        curloc += val
        onval = not onval

    for curdata in datas[::-1]:
        for curfree in frees:
            # If the next free is after the data, we are done looking
            # No future free will work either
            if curfree.initloc >= curdata.locs[0]:
                break
            if curfree.length >= len(curdata.locs):
                st_pt = curfree.take_some(len(curdata.locs))
                curdata.move_all(st_pt)
                break
    
    return sum(block.checksum() for block in datas)

def main(filename):
    with open(filename) as infile:
        lines = (line.strip() for line in infile)
        inline = [int(val) for val in next(lines)]
        
    print(f"Part 1: {part1(inline)}")
    print(f"Part 2: {part2(inline)}")

if __name__ == "__main__":
    main(sys.argv[1])
