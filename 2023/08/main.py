#!/usr/bin/env python3

import re

def parse(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]

    inst = lines[0]
    patt = "([0-9A-Z]{3}) = \(([0-9A-Z]{3}), ([0-9A-Z]{3})\)"
    stops_re = (re.match(patt, line).groups() for line in lines[2:])
    stops = {name: (left, right) for name, left, right in stops_re}

    return inst, stops

def part1(inst, stops):
    cur = "AAA"
    steps = 0
    while cur != "ZZZ":
        step = inst[steps % len(inst)]
        ind = 0 if step == "L" else 1
        cur = stops[cur][ind]
        steps += 1
    
    print(f"Part 1: {steps}")

def find_loop(inst, stops, start):
    cur = start
    path = list()
    steps = 0
    inst_ind = steps % len(inst)
    while (cur, inst_ind) not in path:
        path.append((cur, inst_ind))
        step = inst[inst_ind]
        ind = 0 if step == "L" else 1
        cur = stops[cur][ind]
        steps += 1
        inst_ind = steps % len(inst)

    seq_st = path.index((cur, inst_ind))
    z_pts = [ind for ind, (stop, _) in enumerate(path) if "Z" in stop]
    z_pt = z_pts[-1]
    seq_to_z = z_pt - seq_st + 1
    z_to_end = len(path) - z_pt - 1
    print("fl", seq_st, seq_to_z, z_to_end)
    return seq_st, seq_to_z, z_to_end

def get_posn_on_loop(step, seq_st, seq_to_z, z_to_end):
    no_st = step - seq_st
    on_loop = no_st % (seq_to_z + z_to_end)
    #print("pol", step, no_st, on_loop)
    return on_loop

def part2(inst, stops):
    curs = [name for name in stops.keys() if name[2] == "A"]
    loop_info = [find_loop(inst, stops, cur) for cur in curs]
    big_loop_size = max(seq_to_z for _, seq_to_z, _ in loop_info)
    big_loop_info = [(a,b,c) for a, b, c in loop_info if b == big_loop_size][0]
    
    step = big_loop_info[0] - 1 # seq_st, seq_to_z, z_to_end
    keep_going = True
    #print("li", loop_info)
    coincides = [list() for _ in curs[:-1]]
    while keep_going:
        step += big_loop_info[1]
        posns = [get_posn_on_loop(step, ss, stoz, ztoe) for ss, stoz, ztoe in loop_info]
        at_z = [posn==(stoz-1) for posn, (ss, stoz, stoe) in zip(posns, loop_info)]
        #print("sp", step, posns, at_z)
        for i, _ in enumerate(at_z[:-1]):
            if at_z[i]:
                coincides[i].append(step)
                print(coincides[i])
        #keep_going = not all(at_z)
        keep_going = any(len(coin) < 4 for coin in coincides)
        step += big_loop_info[2]
    print(f"Part 2 is the lcm of these: {[coin[0] for coin in coincides]}")

if __name__ == "__main__":
    #part1(*parse("samp01"))
    #part1(*parse("samp02"))
    #part1(*parse("input"))

    #part2(*parse("samp03"))
    part2(*parse("input"))