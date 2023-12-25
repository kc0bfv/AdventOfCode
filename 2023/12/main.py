#!/usr/bin/env python3

memoized = None

def enumerate_patt_setup(ind, patt, nums):
    global memoized
    memoized = dict()
    if ind % 10 == 0:
        #print(f"Index {ind}")
        pass
    return enumerate_patt(patt, nums)

def enumerate_patt(patt, nums):
    global memoized
    if (len(patt), len(nums)) in memoized:
        return memoized[(len(patt), len(nums))]
    # Recursively enumerate solutions - return the num of solns in this path

    #### Base cases
    # We got to a point where all the needed points were used up
    # If there are more ? - they're . - this is still only one solution
    # If there are more # - this way didn't work so 0 solutions are here
    if len(nums) <= 0:
        if "#" in patt:
            return 0
        return 1
    # More needed points are there, but we are out of pattern.  fail - 0 solns here
    if len(patt) <= 0:
        return 0
    # There aren't enough "#" or "?" remaining to satisfy the need
    if patt.count("#") + patt.count("?") < sum(nums):
        return 0

    #### Normally
    solns_found = 0
    # Allow a ? to go down both paths
    if patt[0] == "." or patt[0] == "?":
        solns_found += enumerate_patt(patt[1:], nums)
    if patt[0] == "#" or patt[0] == "?":
        solns_found += enumerate_patt_in_group(patt[1:], nums[0]-1, list(nums[1:]))
    memoized[(len(patt), len(nums))] = solns_found
    return solns_found

def enumerate_patt_in_group(patt, group_rem, next_nums):
    #### Base cases
    # If no more in the group
    # If this pattern is a "#", then we can't leave the space for next group
    # and this path was a fail.  Otherwise it's ok - eat the space and return
    # to free enum
    if group_rem == 0:
        # If there's no pattern left, let enumerate_patt sort it out
        if len(patt) <= 0:
            return enumerate_patt(patt, next_nums)
        if patt[0] == "." or patt[0] == "?":
            return enumerate_patt(patt[1:], next_nums)
        elif patt[0] == "#":
            return 0
        else:
            raise RuntimeError("Shouldn't be here epig")
    # If no more in the patt but some in the group, we failed
    if len(patt) <= 0:
        return 0

    #### Normal
    # Now we have to finish this group, or we fail
    if patt[0] == "#" or patt[0] == "?":
        return enumerate_patt_in_group(patt[1:], group_rem - 1, next_nums)
    elif patt[0] == ".":
        # We hit a mandatory space before we ended the group - fail
        return 0
    else:
        raise RuntimeError("Shouldn't be here epig2")

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    sp_lines = (line.split(" ") for line in lines)
    patt_nums = [(patt, [int(num) for num in nums.split(",")]) for (patt, nums) in sp_lines]
    enumerated = [enumerate_patt_setup(0, *pn) for pn in patt_nums]
    print(enumerated)
    print(f"Part 1: {sum(enumerated)}")

    """
    for i in range(1, 5):
        patt_nums_2 = [("?".join([patt]*i), nums*i) for (patt, nums) in patt_nums]
        enumerated = [enumerate_patt_2(ind, *pn) for ind, pn in enumerate(patt_nums_2)]
        print(enumerated)
        print(f"Part 2 - {i}: {sum(enumerated)}")
    """
    i=2
    patt_nums_2 = [("?".join([patt]*i), nums*i) for (patt, nums) in patt_nums]
    enumerated_2 = [enumerate_patt_setup(ind, *pn) for ind, pn in enumerate(patt_nums_2)]
    print(enumerated_2) 
    print(f"Part 2 - {i}: {sum(enumerated_2)}")

    i=3
    patt_nums_2 = [("?".join([patt]*i), nums*i) for (patt, nums) in patt_nums]
    enumerated_3 = [enumerate_patt_setup(ind, *pn) for ind, pn in enumerate(patt_nums_2)]
    print(enumerated_3)
    print(f"Part 2 - {i}: {sum(enumerated_3)}")

    divs = [three/two for three, two in zip(enumerated_3, enumerated_2)]
    four_e = [three * div for three, div in zip(enumerated_3, divs)]
    five_e = [four * div for four, div in zip(four_e, divs)]

    print(sum(five_e))

    i=5
    patt_nums_2 = [("?".join([patt]*i), nums*i) for (patt, nums) in patt_nums]
    enumerated_5 = [enumerate_patt_setup(ind, *pn) for ind, pn in enumerate(patt_nums_2)]
    print(enumerated_5)
    print(f"Part 2 - {i}: {sum(enumerated_5)}")

if __name__ == "__main__":
    main("samp01")
    main("input")