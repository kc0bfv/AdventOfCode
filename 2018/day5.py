#!/usr/bin/env python3

import itertools as it

def elim_pairs(indat):
    pairs = it.chain((chr(i)+chr(i-32) for i in range(ord("a"),ord("z")+1)),
                     (chr(i-32)+chr(i) for i in range(ord("a"),ord("z")+1)))
    finds = (indat.find(p) for p in pairs)
    goodfinds = (i for i in finds if i != -1)
    try:
        find = next(goodfinds)
        return indat[:find]+indat[find+2:]
    except StopIteration as e:
        return indat

if __name__ == "__main__":
    with open("input day 5.txt") as f:
        rawdat = f.read()

    origdat = rawdat.strip()
    if len(origdat) != 50000:
        raise RuntimeError("Wrong dat len")

    dat = origdat
    old_dat = ""
    while old_dat != dat:
        old_dat = dat
        dat = elim_pairs(dat)

    print("First: {}".format(len(dat)))

    to_rem = [(chr(i), chr(i-32)) for i in range(ord("a"),ord("z")+1)]
    print(to_rem)
    trans_tables = [str.maketrans({a:None, b:None}) for (a,b) in to_rem]
    touse = dat # use the already reduced data...
    dat_remd = (touse.translate(trns) for trns in trans_tables)
    for remd,remchr in zip(dat_remd, to_rem):
        print("Reming {} len {}".format(remchr, len(remd)))
        dat = remd
        old_dat = ""
        while old_dat != dat:
            old_dat = dat
            dat = elim_pairs(dat)
        print("Fin len {}".format(len(dat)))
