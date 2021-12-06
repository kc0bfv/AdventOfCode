#!/usr/bin/env python3

from sys import argv

def file_dat(filename):
    for line in open(filename,"r").readlines():
        yield line

def prod(val_list):
    if len(val_list) == 0:
        return 1
    return val_list[0] * prod(val_list[1:])

def find_inv(a, b):
    # Find the multiplicative inverse
    # Ref https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm

    r = [a, b]
    s = [1, 0]
    t = [0, 1]
    
    new_r = 1

    while new_r != 0:
        ind = len(r) - 1
        q = int(r[ind-1] / r[ind])
        new_r = r[ind-1] - q * r[ind]
        new_s = s[ind-1] - q * s[ind]
        new_t = t[ind-1] - q * t[ind]

        r.append(new_r)
        s.append(new_s)
        t.append(new_t)

    return s[-2]

def crt_solver(rems, mods):
    # Solves the system of equations for x
    # x = rem0 % mod0
    # x = rem1 % mod1
    # ...

    # Reference https://unacademy.com/lesson/chinese-remainder-theorem/8UL36V3A

    M = prod(mods)
    Ms = [int(M/m) for m in mods]
    invs = [find_inv(Mval, mod) for (Mval, mod) in zip(Ms, mods)]

    return sum(rem * Mval * inv for (rem, Mval, inv) in zip(rems, Ms, invs)) % M


if __name__ == "__main__":
    find_inv(240,46)

    line_r = file_dat(argv[1])
    start_time = int(next(line_r))
    busses_s = next(line_r).split(",")
    busses = [int(bus) for bus in busses_s if bus != "x"]

    time = start_time
    while not any(time % bus == 0 for bus in busses):
        time += 1

    take_busses = [bus for bus in busses if time % bus == 0]
    assert(len(take_busses) == 1)
    print("Part 1: {}".format(take_busses[0] * (time - start_time)))

    busses = [((int(bus)-ind) % int(bus),int(bus)) for ind, bus in enumerate(busses_s) if bus != "x"]

    rems = [bus[0] for bus in busses]
    mods = [bus[1] for bus in busses]

    crt_solution = crt_solver(rems, mods)

    print("Part 2: {}".format(crt_solution))
