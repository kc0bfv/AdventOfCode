#!/usr/bin/env python3

import itertools
import unittest

class MapEntry:
    def __init__(self, line):
        dstnum, srcnum, nlen = (int(num) for num in line.split(" "))
        self.src = range(srcnum, srcnum + nlen)
        self.dst = range(dstnum, dstnum + nlen)
    def map(self, item):
        if item in self.src:
            return self.dst.start + (self.src.index(item))
        raise RuntimeError("Invalid map attempt")
    def map_rng(self, rng):
        # rng is NOT fully inside or equal to me
        if not (self.src.start <= rng.start and rng.stop <= self.src.stop):
            raise RuntimeError("Invalid map_rng attempt made")
        #print(self.src, rng)
        endpt = None
        if self.src.stop != rng.stop:
            endpt = self.dst.start + (self.src.index(rng.stop))
        else:
            endpt = self.dst.stop
        return range(self.dst.start + (self.src.index(rng.start)), endpt)
    def split(self, rng):
        # Case where input is fully before or after the source, or is fully contained by the source
        if rng.stop <= self.src.start or self.src.stop <= rng.start or \
            (rng.start >= self.src.start and rng.stop <= self.src.stop):
            return [rng]
        # Case where the input starts before but overlaps the source
        if rng.start < self.src.start and self.src.start <= rng.stop and rng.stop <= self.src.stop:
            return [range(rng.start, self.src.start), range(self.src.start, rng.stop)]
        # Case where input overlaps end of source
        if rng.start >= self.src.start and rng.start < self.src.stop and rng.stop > self.src.stop:
            return [range(rng.start, self.src.stop), range(self.src.stop, rng.stop)]
        # Case where input fully contains the source
        if rng.start < self.src.start and rng.stop > self.src.stop:
            return [range(rng.start, self.src.start), range(self.src.start, self.src.stop), range(self.src.stop, rng.stop)]
        raise RuntimeError("Invalid split case seen")

class Map(list):
    def map(self, num):
        for me in self:
            if num in me.src:
                return me.map(num)
        return num
    def map_rng(self, rng):
        #print("orig", rng)
        for me in self:
            #print("cand", me.src)
            # rng isn't in me (mapentry) - it's before or after
            if rng.stop <= me.src.start or me.src.stop <= rng.start:
                continue
            # rng is fully inside or equal to me
            if me.src.start <= rng.start and rng.stop <= me.src.stop:
                return me.map_rng(rng)
            raise RuntimeError("Invalid map_rng state")
        return rng

    def split(self, inrng):
        cur_rngs = [inrng]
        for me in self:
            new_rngs = list()
            for rng in cur_rngs:
                new_rngs.extend(me.split(rng))
            cur_rngs = new_rngs
        return cur_rngs

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]

    seeds = [int(num) for num in lines[0].split(" ")[1:]]

    maps = list()

    cur_map = None
    for line in lines[2:]:
        if ":" in line:
            cur_map = Map()
        elif line == "":
            maps.append(cur_map)
            cur_map = None
        else:
            cur_map.append(MapEntry(line))

    if cur_map is not None:
        maps.append(cur_map)
        cur_map = None

    cur_nums = list(seeds)
    for mapping in maps:
        cur_nums = [mapping.map(num) for num in cur_nums]
    
    print(f"Part 1: {min(cur_nums)}")

    seed_iter = iter(seeds)
    cur_seed_rngs = [range(st, st + next(seed_iter)) for st in seed_iter]
    for mapping in maps:
        #print(cur_seed_rngs)
        split_rngs = itertools.chain.from_iterable(mapping.split(rng) for rng in cur_seed_rngs)
        cur_seed_rngs = [mapping.map_rng(rng) for rng in split_rngs]
    #print(cur_seed_rngs)
    rng_starts = [rng.start for rng in cur_seed_rngs]
    print(f"Part 2: {min(rng_starts)}")
    
if __name__ == "__main__":
    main("samp01")
    main("input")

class TestMap(unittest.TestCase):
    def test_rangeSplit(self):
        # Source 40-99, dest 400-459, 
        me = MapEntry("400 40 60")
        splits = [
            # Fully before
            (range(10, 30),[range(10, 30)]),
            (range(10, 40),[range(10, 40)]),
            # Fully after
            (range(100, 150),[range(100, 150)]),
            (range(101, 130),[range(101, 130)]),
            # Fully contained
            (range(50, 60),[range(50, 60)]),
            (range(40, 50),[range(40, 50)]),
            (range(60, 100),[range(60, 100)]),
            (range(40, 100),[range(40, 100)]),
            # Overlaps beginning
            (range(39, 41),[range(39, 40), range(40, 41)]),
            (range(30, 50),[range(30, 40), range(40, 50)]),
            (range(10, 41),[range(10, 40), range(40, 41)]),
            # Overlaps ending
            (range(40, 110),[range(40, 100), range(100, 110)]),
            (range(99, 101),[range(99, 100), range(100, 101)]),
            (range(90, 110),[range(90, 100), range(100, 110)]),
            # Fully contains
            (range(39, 101),[range(39, 40), range(40, 100), range(100, 101)]),
            (range(0,150),[range(0, 40), range(40, 100), range(100, 150)]),
        ]
        results = [me.split(split[0]) for split in splits]
        for result, (inval, expected) in zip(results, splits):
            self.assertEqual(result, expected)

    def test_mapSplit(self):
        tmap = Map()
        # Source 40-59, dest 400-419
        tmap.append(MapEntry("400 40 20"))
        # Source 60-79, dest 500-519
        tmap.append(MapEntry("500 60 20"))
        # Source 90-99, dest 600-609
        tmap.append(MapEntry("600 90 10"))

        splits = [
            # Fully before
            (range(10, 30),[range(10, 30)]),
            (range(10, 40),[range(10, 40)]),
            # Fully after
            (range(100, 150),[range(100, 150)]),
            (range(101, 130),[range(101, 130)]),
            # Fully contained
            (range(50, 60),[range(50, 60)]),
            (range(40, 50),[range(40, 50)]),
            (range(60, 100),[range(60, 80), range(80, 90), range(90, 100)]),
            (range(40, 100),[range(40, 60), range(60, 80), range(80, 90), range(90, 100)]),
            # Overlaps beginning
            (range(39, 41),[range(39, 40), range(40, 41)]),
            (range(30, 92),[range(30, 40), range(40, 60), range(60, 80), range(80, 90), range(90, 92)]),
            (range(10, 41),[range(10, 40), range(40, 41)]),
            # Overlaps ending
            (range(40, 110),[range(40, 60), range(60, 80), range(80, 90), range(90, 100), range(100, 110)]),
            (range(99, 101),[range(99, 100), range(100, 101)]),
            (range(90, 110),[range(90, 100), range(100, 110)]),
            # Fully contains
            (range(39, 101),[range(39, 40), range(40, 60), range(60, 80), range(80, 90), range(90, 100), range(100, 101)]),
            (range(0,150),[range(0, 40), range(40, 60), range(60, 80), range(80, 90), range(90, 100), range(100, 150)]),
        ]
        
        results = [tmap.split(split[0]) for split in splits]
        for result, (inval, expected) in zip(results, splits):
            self.assertEqual(result, expected)

    def test_mapMap(self):
        tmap = Map()
        # Source 40-59, dest 400-419
        tmap.append(MapEntry("400 40 20"))
        # Source 60-79, dest 500-519
        tmap.append(MapEntry("500 60 20"))
        # Source 90-99, dest 600-609
        tmap.append(MapEntry("600 90 10"))

        splits = [
            # Fully before
            (range(10, 30),),
            (range(10, 40),),
            # Fully after
            (range(100, 150),),
            (range(101, 130),),
            # Fully contained
            (range(50, 60),),
            (range(40, 50),),
            (range(60, 100),),
            (range(40, 100),),
            # Overlaps beginning
            (range(39, 41),),
            (range(30, 92),),
            (range(10, 41),),
            # Overlaps ending
            (range(40, 110),),
            (range(99, 101),),
            (range(90, 110),),
            # Fully contains
            (range(39, 101),),
            (range(0,150),),
        ]

        results = [[tmap.map_rng(rng) for rng in tmap.split(split[0])] for split in splits]

        for result, (inval, ) in zip(results, splits):
            print(f"{inval}: {result}")