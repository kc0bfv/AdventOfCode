#!/usr/bin/env python3

def read(filename):
    with open(filename, "r") as fin:
        return [int(line) for line in fin]

def search(nums):
    for st_ind, st_val in enumerate(nums[:-1]):
        for nx_val in nums[st_ind+1:]:
            if nx_val + st_val == 2020:
                print(nx_val * st_val)

def search2(nums):
    for st_ind, st_val in enumerate(nums[:-1]):
        for nx_ind_pt, nx_val in enumerate(nums[st_ind+1:]):
            nx_ind = nx_ind_pt + 1 + st_ind
            for ls_ind_pt, ls_val in enumerate(nums[nx_ind+1:]):
                if nx_val + st_val + ls_val == 2020:
                    print(nx_val * st_val * ls_val )

if __name__ == "__main__":
    nums = read("input.txt")
    search(nums)
    search2(nums)
