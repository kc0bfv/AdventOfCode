#!/usr/bin/env python3

def dist(gal, gal2):
    return abs(gal2[0]-gal[0]) + abs(gal2[1]-gal[1])

def main(filename, exp_fac):
    with open(filename) as f:
        lines = [line.strip() for line in f]

    gals = list()
    rows_unused = set(range(len(lines)))
    cols_unused = set(range(len(lines[0])))
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char == "#":
                gals.append((x, y))
                try: rows_unused.remove(y)
                except KeyError: pass
                try: cols_unused.remove(x)
                except KeyError: pass
    
    row_shift = list(0 for i in range(len(lines)))
    col_shift = list(0 for i in range(len(lines[0])))
    for row_unused in rows_unused:
        for ind in range(row_unused + 1, len(lines)):
            row_shift[ind] += exp_fac
    for col_unused in cols_unused:
        for ind in range(col_unused + 1, len(lines[0])):
            col_shift[ind] += exp_fac

    new_gals = [(x + col_shift[x], y + row_shift[y]) for (x,y) in gals]
    
    total = 0
    for ind, gal in enumerate(new_gals):
        for gal2 in new_gals[ind+1:]:
            #print(gal, gal2, dist(gal, gal2))
            total += dist(gal, gal2)

    print(f"Exp fac {exp_fac} total: {total}")

if __name__ == "__main__":
    main("samp01", 999999)
    main("input", 999999)