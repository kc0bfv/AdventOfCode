#!/usr/bin/env python3

from collections import defaultdict
import sys

def search(lines, letters, row, col, row_d, col_d):
    # Base case
    if len(letters) <= 0:
        return True

    n_row = row + row_d
    n_col = col + col_d
    if not (0 <= n_row < len(lines) and 0 <= n_col < len(lines[0])):
        return False
    if lines[n_row][n_col] != letters[0]:
        return False

    return search(lines, letters[1:], n_row, n_col, row_d, col_d)

def main(filename):
    with open(filename) as infile:
        lines = [line.strip() for line in infile]

    # Part 1
    found = list()
    for row, line in enumerate(lines):
        for col, char in enumerate(line):
            if char != "X":
                continue
            for row_d in range(-1, 2):
                for col_d in range(-1, 2):
                    if row_d == 0 and col_d == 0:
                        pass
                    if search(lines, "MAS", row, col, row_d, col_d):
                        found.append((row, col, row_d, col_d))

    found2 = list()
    for row, line in enumerate(lines):
        for col, char in enumerate(line):
            if char != "A" or row == 0 or row >= len(lines) - 1 or \
                col == 0 or col >= len(line) - 1:
                continue
            if ((lines[row-1][col-1] == "M" and lines[row+1][col+1] == "S") or \
                (lines[row-1][col-1] == "S" and lines[row+1][col+1] == "M")) and \
                ((lines[row-1][col+1] == "M" and lines[row+1][col-1] == "S") or \
                (lines[row-1][col+1] == "S" and lines[row+1][col-1] == "M")):
                found2.append((row, col))
                

    print(f"Part 1: {len(found)}")
    print(f"Part 2: {len(found2)}")

if __name__ == "__main__":
    main(sys.argv[1])
