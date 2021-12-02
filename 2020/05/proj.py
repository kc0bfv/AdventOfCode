#!/usr/bin/env python3

def changeem(line):
    trans = str.maketrans({"F":"0", "L":"0", "B":"1", "R": "1"})
    transl = line.translate(trans)
    row = int(transl[:7], 2)
    col = int(transl[7:], 2)
    return (row, col, (row*8+col))

def find_missing(seatids):
    seatids.sort()
    last = seatids[0]
    for seat in seatids[1:]:
        if seat - last != 1:
            return (seat, last)
        last = seat

if __name__ == "__main__":
    lines = [a.strip() for a in open("input.txt","r").readlines()]
    seats = [changeem(line) for line in lines]

    seatids = [seat[2] for seat in seats]
    print("Part 1: {}".format(max(seatids)))

    print("Part 2: {}".format(find_missing(seatids)))
