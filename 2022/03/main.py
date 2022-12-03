#!/usr/bin/env python3

from itertools import zip_longest

def build_sack(line):
    midpt = int( len(line)/2 )
    return (set( line[:midpt] ), set( line[midpt:] ))
    
def find_common_orig(sack):
    compa, compb = sack
    inters = compa.intersection(compb)
    assert(len(inters) == 1)
    return inters.pop()

def get_priority(common):
    if common > 'Z':
        # is lower
        return ord(common) - ord('a') + 1
    else:
        # is upper
        return ord(common) - ord('A') + 27

# python itertools...
def grouper(iterable, n, *, incomplete='fill', fillvalue=None):
    "Collect data into non-overlapping fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, fillvalue='x') --> ABC DEF Gxx
    # grouper('ABCDEFG', 3, incomplete='strict') --> ABC DEF ValueError
    # grouper('ABCDEFG', 3, incomplete='ignore') --> ABC DEF
    args = [iter(iterable)] * n
    if incomplete == 'fill':
        return zip_longest(*args, fillvalue=fillvalue)
    if incomplete == 'strict':
        return zip(*args, strict=True)
    if incomplete == 'ignore':
        return zip(*args)
    else:
        raise ValueError('Expected fill, strict, or ignore')

def find_common(group):
    # fun with iterators - this isn't exactly how I first solved it
    # but is how I hoped it was solvable.  I had to play with the
    # iterators to get this
    # now it's generic enough that I no longer need the original find_common
    as_sets = (set(g) for g in group)
    fin = (next(as_sets)).intersection(*as_sets)
    assert(len(fin) == 1)
    return fin.pop()

def main(filename):
    with open(filename, "r") as f:
        lines = [l.strip() for l in f]

    sacks = (build_sack(line) for line in lines)
    commons = (find_common(sack) for sack in sacks)
    pris = (get_priority(common) for common in commons)

    print(f"Part 1 {sum(pris)}")

    grouped_lines = grouper(lines, 3, incomplete="strict")
    commons_2 = (find_common(group) for group in grouped_lines)
    pris_2 = (get_priority(common) for common in commons_2)

    print(f"Part 2 {sum(pris_2)}")

if __name__ == "__main__":
    assert(get_priority('p') == 16)
    assert(get_priority('L') == 38)

    main("input.txt")
