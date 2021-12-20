#!/usr/bin/env python3

import itertools as it
import sys

"""
 000
1   2
1   2
1   2
 333
4   5
4   5
4   5
 666
"""

def process_line(line):
    prev, post = line.split("|")
    signals = prev.strip().split(" ")
    output = post.strip().split(" ")
    return signals, output

def build_translation(codes, mute5, mute6):
    translation = {i: None for i in range(10)}
    mute5_it = iter(mute5)
    mute6_it = iter(mute6)
    for code in codes:
        if len(code) == 2:
            decoded = 1
        elif len(code) == 3:
            decoded = 7
        elif len(code) == 4:
            decoded = 4
        elif len(code) == 5:
            decoded = next(mute5_it)
        elif len(code) == 6:
            decoded = next(mute6_it)
        elif len(code) == 7:
            decoded = 8
        assert(translation[decoded] is None)
        translation[decoded] = code
    return translation

def map_trans_to_segs(trans):
    invalid = False

    # Start by assuming that any letter can map to any segment
    seg_poss = ["abcdefg" for _ in range(7)]
    # The list of segments used to display each value
    segs_used = [
        [0,1,2,4,5,6], #0
        [2, 5], [0,2,3,4,6], #1, 2
        [0,2,3,5,6], #3
        [1,2,3,5], #4
        [0,1,3,5,6],#5
        [0,1,3,4,5,6],
        [0,2,5],
        [0,1,2,3,4,5,6],
        [0,1,2,3,5,6], #9
    ]
    # Based on the translation, eliminate seg possibilities that don't fit
    for decoded, code in trans.items():
        if code is None:
            continue
        su = segs_used[decoded]
        for seg in su:
            seg_poss[seg] = [poss for poss in seg_poss[seg] if poss in code]


    changed = True
    while changed == True:
        changed = False
        # Get ready to eliminate single wires from any other segments...
        singles = [lst[0] for lst in seg_poss if len(lst) == 1]
        # First make sure we don't have any wires that map to two single segments...
        if len(set(singles)) != len(singles):
            invalid = True
        # Now eliminate single wires from any other segments...
        for single in singles:
            for ind in range(len(seg_poss)):
                if len(seg_poss[ind]) > 1:
                    orig_len = len(seg_poss[ind])
                    seg_poss[ind] = [val for val in seg_poss[ind] if val != single]
                    if len(seg_poss[ind]) != orig_len:
                        changed = True

    return invalid, seg_poss
        

def map_and_check_consistent(trans, codes):
    is_inval, segment_poss = map_trans_to_segs(trans)
    precise = all(len(seg) == 1 for seg in segment_poss)
    if not is_inval and precise:
        return segment_poss, True
    return segment_poss, False

def reverse_map(mapping):
    # The list of segments used to display each value
    segs_used = [
        [0,1,2,4,5,6], #0
        [2, 5], [0,2,3,4,6], #1, 2
        [0,2,3,5,6], #3
        [1,2,3,5], #4
        [0,1,3,5,6],#5
        [0,1,3,4,5,6],
        [0,2,5],
        [0,1,2,3,4,5,6],
        [0,1,2,3,5,6], #9
    ]
    codes = ["".join(sorted("".join(mapping[i]) for i in segs)) for segs in segs_used]
    return codes

def segs_to_num(code, codes):
    code_n = "".join(sorted(code))
    return codes.index(code_n)

def calc_line(signals, output):
    codes = list({"".join(sorted(l)) for l in signals+output})
    mutes_5 = list(it.permutations([2,3,5]))
    mutes_6 = list(it.permutations([0,6,9]))
    found_trans = None
    found_map = None
    for mute5 in mutes_5:
        for mute6 in mutes_6:
            trans = build_translation(codes, mute5, mute6)
            seg_map, consistent = map_and_check_consistent(trans, codes)
            if consistent:
                if found_trans is None:
                    found_trans = trans
                    found_map = seg_map
                else:
                    print("ERROR")
    assert(found_trans is not None)
    rev_map = reverse_map(found_map)
    num = int("".join([str(segs_to_num(out, rev_map)) for out in output]))
    return num

if __name__ == "__main__":
    lines = [process_line(line) for line in open(sys.argv[1], "r").readlines()]

    count = 0
    for _, outputs in lines:
        for output in outputs:
            if len(output) in [2,3,4,7]:
                count += 1

    print("Part 1", count)

    calced = [calc_line(sig, out) for sig,out in lines]
    print("Part 2", sum(calced))
