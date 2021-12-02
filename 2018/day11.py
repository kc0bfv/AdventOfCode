#!/usr/bin/env python3

import itertools as it

SERIALNO = 8

def calc_pw_lvl(x, y, ser=SERIALNO):
    rackid = x + 10
    powerlevel = ((rackid * y) + ser) * rackid

    hunnerds = int(powerlevel / 100) % 10
    return hunnerds - 5

def build_grid(xmax=300, ymax=300, ser=SERIALNO):
    return [[calc_pw_lvl(x+1, y+1, ser) for x in range(xmax)]
            for y in range(ymax)]

def subgrid(grid, xmin, xmax, ymin, ymax):
    return [[grid[y][x] for x in range(xmin-1, xmax-1)] for y in range(ymin-1, ymax-1)]

def sum_horizon(grid, x, y, old_dim):
    return sum([grid[y+i][x+old_dim] for i in range(old_dim)] + \
            [grid[y+old_dim][x+i] for i in range(old_dim)] + \
            [grid[y+old_dim][x+old_dim]])

def find_biggest_sums(sums):
    biggest_sum = -1000000
    big_sum_loc = -1

    max_val, max_line = max((max(line), ind) for ind, line in enumerate(sums))
    return (sums[max_line].index(max_val) + 1, max_line + 1, max_val)
    
def find_biggest_sq(grid, max_dim=20):
    sums = [
            [sum(sum(grid[y+j][x:x+1]) for j in range(1))
                for x in range(len(grid[0]) - (1-1))]
            for y in range(len(grid) - (1-1))
            ]

    biggest = find_biggest_sums(sums)
    biggest_dim = 1

    for idim in range(1, max_dim):
        new_dim = idim+1
        old_dim = idim
        next_sum = [[sums[y][x] + sum_horizon(grid, x, y, old_dim)
            for x in range(len(grid[0]) - (new_dim-1))]
            for y in range(len(grid) - (new_dim-1))]

        next_biggest = find_biggest_sums(next_sum)

        sums = next_sum
        if next_biggest[2] > biggest[2]:
            biggest = next_biggest
            biggest_dim = new_dim

    return (biggest, biggest_dim)

if __name__ == "__main__":
    tests = [
            ((3, 5, 8), 4),
            ((122, 79, 57), -5),
            ((217, 196, 39), 0),
            ((101, 153, 71), 4)
            ]
    for inputs, output in tests:
        if calc_pw_lvl(*inputs) != output:
            print("Fail: {}".format(inputs))

    grid = build_grid(ser=1309)
    for row in subgrid(grid, 32, 37, 44, 49):
        print(" ".join("{: 2}".format(i) for i in row))

    result = find_biggest_sq(grid, 300)
    print(result)



    """
    big_sqs = [find_biggest_sq(grid, i) for i in range(40)]
    big_sqx, big_sqy, big_sqval = list(zip(*big_sqs))
    big_sqpos = big_sqval.index(max(big_sqval))
    print(big_sqx[big_sqpos], big_sqy[big_sqpos], big_sqpos, max(big_sqval))
    """



