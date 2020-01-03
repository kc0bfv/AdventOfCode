#!/usr/bin/env python3

LASTGEN = 50000000000

def doxform(tfs, curstate):
    to_lookup = (curstate[i:i+5] for i in range(len(curstate) - 4))
    beg_vals = curstate[0:2]
    end_vals = curstate[-2:]
    fin_state = (tfs[lookup] for lookup in to_lookup)
    return beg_vals + "".join(fin_state) + end_vals

if __name__ == "__main__":
    with open("input day 12.txt") as f:
        lines = [line for line in f]

    initstate = lines[0].split(": ")[1].strip()
    tflines = [line.strip().split(" => ") for line in lines[2:]]
    tfs = {init: result for (init, result) in tflines}

    curstate = initstate
    first_ind = 0
    earlier_val = 0
    for i in range(LASTGEN):
        begempty = all(curstate[i] == "." for i in range(5))
        endempty = all(curstate[i] == "." for i in range(-5,0))
        if not begempty:
            curstate = "....." + curstate
            first_ind -= 5
        if not endempty:
            curstate = curstate + "....."
        curstate = doxform(tfs, curstate)

        filted_hash = filter(lambda x: x[0] == "#",
                zip(curstate, range(first_ind, first_ind+len(curstate))))
        val = sum(filt[1] for filt in filted_hash)

        print("Gen {}: {} {}".format(i, val, val-earlier_val))
        earlier_val = val
