#!/usr/bin/env python3

class Elf:
    def __init__(self, elf_sects):
        split = elf_sects.split("-")
        self.start, self.end = (int(s) for s in split)

    def does_consume(self, other):  
        return self.start <= other.start and self.end >= other.end

    def has_no_overlap(self, other):
        return self.end < other.start or self.start > other.end

def build_pair(line):
    return tuple(Elf(sp) for sp in line.split(","))

def is_fully_consumed(elf): 
    return elf[0].does_consume(elf[1]) or elf[1].does_consume(elf[0])

def has_any_overlap(elf):
    return not elf[0].has_no_overlap(elf[1])

def iter_len(iterator):
    return sum(1 for _ in iterator)

def main(filename):
    with open(filename, "r") as f:
        lines = [l.strip() for l in f]

    pairs = [build_pair(l) for l in lines]
    is_full_c = (p for p in pairs if is_fully_consumed(p))
    has_overlap = (p for p in pairs if has_any_overlap(p))

    print(f"Part 1: {iter_len(is_full_c)}")
    print(f"Part 2: {iter_len(has_overlap)}")



if __name__ == "__main__":
    main("input.txt")
