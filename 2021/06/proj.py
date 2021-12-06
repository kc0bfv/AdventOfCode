#!/usr/bin/env python3

from sys import argv

def bin_fish(age_list):
    assert(max(age_list) <= 8)
    assert(min(age_list) >= 0)
    return {i: age_list.count(i) for i in range(9)}

def age_bins(bins):
    bins = dict(bins)
    bins[7] += bins[0]
    bins[9] = bins[0]
    for i in range(9):
        bins[i] = bins[i+1]
    bins[9] = 0
    return bins

if __name__ == "__main__":
    ages = [int(age) for age in open(argv[1]).readlines()[0].split(",")]

    bins = bin_fish(ages)

    for i in range(80):
        bins = age_bins(bins)

    print("Part 1:", sum(bins.values()))

    for i in range(256-80):
        bins = age_bins(bins)

    print("Part 2:", sum(bins.values()))
