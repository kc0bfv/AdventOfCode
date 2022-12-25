#!/usr/bin/env python3

import math

def from_snafu(inval):
    lookup = {"2": 2, "1": 1, "0": 0, "-": -1, "=": -2}
    retval = 0
    for ind, char in enumerate(reversed(inval)):
        retval += lookup[char] * (5 ** ind)
    return retval

def to_quinary(inval):
    st_exp = math.floor(math.log(inval, 5))
    cur_val = inval
    retval = list()
    for exp in range(st_exp, -1, -1):
        inst = math.floor( cur_val / (5 ** exp) )
        retval.append(inst)
        cur_val -= inst * (5 ** exp)

    return retval

def to_snafu_from_quin(quinval):
    outval = list()
    carry = 0
    for ind, val in enumerate(reversed(quinval)):
        withcarry = int(val) + carry
        if withcarry in [0, 1, 2]:
            outval.append(str(withcarry))
            carry = 0
        elif withcarry == 3:
            outval.append("=")
            carry = 1
        elif withcarry == 4:
            outval.append("-")
            carry = 1
        elif withcarry == 5:
            outval.append("0")
            carry = 1
        else:
            raise RuntimeError(val, carry)
    if carry > 0:
        outval.append(str(carry))
    return "".join(reversed(outval))

def to_snafu(inval):
    return to_snafu_from_quin(to_quinary(inval))

def main(filename):
    with open(filename, "r") as f:
        vals = [from_snafu(l.strip()) for l in f]

    print(vals, sum(vals))

    print(f"Part 1: {to_snafu(sum(vals))}")

if __name__ == "__main__":
    for ind in range(1, 3000):
        as_snaf = to_snafu(ind)
        from_snaf = from_snafu(as_snaf)
        assert(ind == from_snaf)

    main("samp")
    main("input.txt")
