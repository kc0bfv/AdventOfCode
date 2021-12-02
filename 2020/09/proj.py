#!/usr/bin/env python3

def part1(preamble, data):
    for cur_ind in range(preamble, len(data)):
        cur_val = data[cur_ind]
        lookset = data[(cur_ind-preamble):cur_ind]
        found = None
        for val in lookset:
            needed = cur_val - val
            if needed != val and needed in lookset:
                found = (val, needed)
        if found is None:
            print("Part 1: {}".format(cur_val))
            return cur_val

def part2_find_run(invalid, data):
    for st_ind in range(len(data)-1, -1, -1):
        cur_sum = data[st_ind]
        for end_ind in range(st_ind-1, -1, -1):
            cur_sum += data[end_ind]
            if cur_sum == invalid:
                return (st_ind, end_ind)
            if cur_sum > invalid:
                break
            

if __name__ == "__main__":
    filename = "input.txt"
    preamble = 25

    data = [int(v) for v in open(filename, "r").read().split()]
    invalid = part1(preamble, data)

    run_inds = part2_find_run(invalid, data)
    run = data[run_inds[1]:run_inds[0]+1]

    print("Part 2: {}".format(min(run) + max(run)))
