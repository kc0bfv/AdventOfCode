#!/usr/bin/env python3

def part1_diffs(data):
    paired = zip(data[:-1], data[1:])
    diffs = [b-a for (a, b) in paired]

    return diffs

def part2(data):
    ways_d = dict()

    ways_d[0] = 1

    for dat in data[1:]:
        opts = [dat-3, dat-2, dat-1]
        ways_d[dat] = sum(ways_d.get(opt, 0) for opt in opts)

    return ways_d[max(data)]

if __name__ == "__main__":
    filename = "input.txt"

    data = [int(i) for i in open(filename, "r").read().split()]
    data.sort()
    data.append(max(data) + 3)
    data.insert(0, 0)

    diffs = part1_diffs(data)

    o = diffs.count(1)
    t = diffs.count(3)

    print("Part 1: {}".format(o*t))

    print("Part 2: {}".format(part2(data)))
