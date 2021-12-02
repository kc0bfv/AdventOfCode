#!/usr/bin/env python3

if __name__ == "__main__":
    filename = "input.txt"
    data = [int(a) for a in open(filename, "r").read().split()]

    prev = data[0]
    count = 0
    for dat in data[1:]:
        if dat > prev:
            count += 1
        prev = dat

    print("Part 1: {}".format(count))

    prev = sum(data[0:3])
    count = 0
    for ind in range(4, len(data) + 1):
        cur = sum(data[ind-3:ind])
        if cur > prev:
            count += 1
        prev = cur

    print("Part 2: {}".format(count))
