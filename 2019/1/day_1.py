#!/usr/bin/env python3

def get_first():
    with open("1_input", "r") as f:
        masses = [int(line) for line in f]

    fins = [int(mass / 3) - 2 for mass in masses]

    total_req = sum(fins)
    print("First: {}".format(total_req))

    return total_req

def get_second():
    with open("1_input", "r") as f:
        masses = [int(line) for line in f]

    fins = [int(mass / 3) - 2 for mass in masses]
    furthered = [refine(fin) for fin in fins]

    total_req = sum(furthered)
    print("Second: {}".format(total_req))

    return total_req

def refine(val):
    cur_amt = val
    while cur_amt > 0:
        cur_amt = int(cur_amt / 3) - 2
        if cur_amt < 0:
            cur_amt = 0
        val += cur_amt
    return val

get_second()
