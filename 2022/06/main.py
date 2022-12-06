#!/usr/bin/env python3

def parts(line, count):
    for ind in range(count, len(line)):
        if len(set(line[ind-count:ind])) == count:
            return ind


def main(filename):
    with open(filename, "r") as f:
        line = f.readlines()[0].strip()

    print(f"Part 1: {parts(line, 4)}")
    print(f"Part 2: {parts(line, 14)}")


if __name__ == "__main__":
    main("samp")
    main("samp2")
    main("input.txt")
