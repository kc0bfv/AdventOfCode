#!/usr/bin/env python3

def compare_lists(a, b):
    return sum(1 for (i, j) in zip(a, b) if i!=j)

def check_refl(ind, lists, needed_diff):
    ind_1, ind_2 = ind, ind + 1
    checked_one = False
    accum_diff = 0
    while ind_1 >= 0 and ind_2 < len(lists):
        checked_one = True
        accum_diff += compare_lists(lists[ind_1], lists[ind_2])
        if accum_diff > needed_diff:
            return False
        ind_1 -= 1
        ind_2 += 1
    if accum_diff != needed_diff:
        return False
    return checked_one

def find_refl(needed_diff, rows, cols):
    # Return (ind, "V" | "H")
    # Look for horiz
    for ind in range(len(rows) - 1):
        if check_refl(ind, rows, needed_diff):
            return (ind, "H")
    # Look for vert
    for ind in range(len(cols) - 1):
        if check_refl(ind, cols, needed_diff):
            return (ind, "V")
    raise RuntimeError("No refl found")

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]

    maps = list()
    rows = list()
    for line in lines:
        if line == "":
            cols = [[row[col] for row in rows] for col in range(len(rows[0]))]
            maps.append((rows, cols))
            rows = list()
            continue
        rows.append(line)
    cols = [[row[col] for row in rows] for col in range(len(rows[0]))]
    maps.append((rows, cols))
    
    refls = [find_refl(0, *m) for m in maps]
    print(refls)
    p1_vals = [(ind+1) if dir == "V" else ((ind+1) * 100) for ind, dir in refls]
    print(f"Part 1: {sum(p1_vals)}")

    refls = [find_refl(1, *m) for m in maps]
    print(refls)
    p2_vals = [(ind+1) if dir == "V" else ((ind+1) * 100) for ind, dir in refls]
    print(f"Part 2: {sum(p2_vals)}")
    
if __name__ == "__main__":
    main("samp01")
    main("input")