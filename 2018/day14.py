#!/usr/bin/env python3

#ENDPOINT = 286051
ENDPOINT = [2,8,6,0,5,1]

def new_vals(vals, elf1, elf2):
    full_val = vals[elf1] + vals[elf2]
    fst = int(full_val / 10)
    snd = full_val % 10
    if fst == 0:
        return [snd]
    else:
        return [fst, snd]

if __name__ == "__main__":
    vals = [3,7]
    elf1, elf2 = 0, 1

    #while len(vals) < ENDPOINT + 10:
    l,m = len(ENDPOINT), len(ENDPOINT)-1
    while vals[-l:] != ENDPOINT and vals[-m:-1] != ENDPOINT:
        #vals += new_vals(vals, elf1, elf2)
        full_val = vals[elf1] + vals[elf2]
        fst, snd = int(full_val / 10), full_val % 10
        vals.extend((snd,) if fst == 0 else (fst, snd))
        elf1 = (elf1 + vals[elf1] + 1) % len(vals)
        elf2 = (elf2 + vals[elf2] + 1) % len(vals)

    print("".join(str(val) for val in vals[-10:]))
    print(len(vals)-len(ENDPOINT))
