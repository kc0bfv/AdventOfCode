#!/usr/bin/env python3

def recur_next_val(intlist):
    if intlist[0] == intlist[1] == 0:
        return 0
    iterlist = iter(intlist)
    subtrd = [v - next(iterlist) for v in intlist[1:]]
    #print(subtrd)
    return intlist[-1] + recur_next_val(subtrd)

def recur_prev_val(intlist):
    if intlist[0] == intlist[1] == 0:
        return 0
    iterlist = iter(intlist)
    subtrd = [v - next(iterlist) for v in intlist[1:]]
    print(subtrd)
    return intlist[0] - recur_prev_val(subtrd)

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    intvals = [[int(n) for n in line.split()] for line in lines]

    nexts = [recur_next_val(intlist) for intlist in intvals]
    print(nexts)
    print(f"Part 1: {sum(nexts)}")

    nexts = [recur_prev_val(intlist) for intlist in intvals]
    print(nexts)
    print(f"Part 2: {sum(nexts)}")

if __name__ == "__main__":
    main("samp01")
    main("input")