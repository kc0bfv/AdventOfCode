#!/usr/bin/env python3

from collections import defaultdict

def dist(star1, star2):
    return sum(abs(a-b) for (a, b) in zip(star1, star2))

def combine_const_to_combine(const_to_combine):

    all_consts_here = set()
    for c1, c2 in const_to_combine:
        all_consts_here.update([c1, c2])

    constscombined = list()
    for c1, c2 in const_to_combine:
        found_already = None
        for ccsetind in range(len(constscombined)):
            if c1 in constscombined[ccsetind] or \
                    c2 in constscombined[ccsetind]:
                if found_already is not None:
                    constscombined[found_already].update(
                            constscombined[ccsetind])
                    constscombined[ccsetind] = set()
                else:
                    found_already = ccsetind

                constscombined[found_already].update([c1, c2])
        if found_already is None:
            constscombined.append(set([c1, c2]))

    print(constscombined)
    outlist = list()
    for ccset in constscombined:
        if ccset != set():
            firval = min(ccset)
            for secval in (i for i in ccset if i != firval):
                outlist.append((firval, secval))

    print(outlist)
    testset = set()
    for fval,sval in outlist:
        testset.add(fval)
        if sval in testset:
            raise RuntimeError("second val found twice in testset")
        testset.add(sval)

    if testset != all_consts_here:
        print(testset, all_consts_here)
        raise RuntimeError("Missing const!")

    return outlist


if __name__ == "__main__":
    with open("input day 25.txt") as f:
        lines = [line.strip() for line in f]

    stars = [tuple(int(i) for i in line.split(",")) for line in lines]

    constellation = [None for star in stars]

    const_to_combine = set()

    next_constellation = 0
    for ind, star in enumerate(stars):
        if constellation[ind] is None:
            constellation[ind] = next_constellation
            next_constellation += 1
        for alind2, star2 in enumerate(stars[ind+1:]):
            ind2 = alind2 + ind + 1
            #print(star, star2, dist(star, star2))
            if dist(star, star2) <= 3:
                if constellation[ind2] is not None:
                    c1 = constellation[ind]
                    c2 = constellation[ind2]
                    if c1 != c2:
                        const_to_combine.add((min(c1, c2), max(c1, c2)))
                else:
                    constellation[ind2] = constellation[ind]

    print(constellation)
    print(const_to_combine)

    changes_made = True
    while changes_made:
        new_const_to_combine = combine_const_to_combine(const_to_combine)
        #if const_to_combine == new_const_to_combine:
        #    changes_made = False
        changes_made = False
        const_to_combine = new_const_to_combine

    print(constellation)
    print(const_to_combine)

    for constdest, constsrc in const_to_combine:
        for ind in range(len(constellation)):
            if constellation[ind] == constsrc:
                constellation[ind] = constdest

    print(constellation)

    print(len(set(constellation)))
