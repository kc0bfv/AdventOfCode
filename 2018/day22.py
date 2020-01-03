#!/usr/bin/env python3

depth = 3066
target = (13, 726)

#depth = 510
#target = (10, 10)

ROCKY = "."
WET = "="
NARROW = "|"
MOUTH = "M"
TARGET = "T"

TYPES = {0: ROCKY, 1: WET, 2: NARROW} 

CLIMBING = "C"
TORCH = "T"
NEITHER = "N"

GEAR = {CLIMBING: 0, TORCH: 1, NEITHER: 2}

INVALID_EQUIP = {ROCKY: GEAR[NEITHER], WET: GEAR[TORCH], NARROW: GEAR[CLIMBING]}

calced_indexes = dict()
def geologic_index(pt):
    if pt not in calced_indexes:
        if pt == (0, 0):
            calced_indexes[pt] = 0
        elif pt == target:
            calced_indexes[pt] = 0
        elif pt[1] == 0:
            calced_indexes[pt] = pt[0] * 16807
        elif pt[0] == 0:
            calced_indexes[pt] = pt[1] * 48271
        else:
            calced_indexes[pt] = erosion_level((pt[0]-1, pt[1])) * \
                    erosion_level((pt[0], pt[1]-1))
    return calced_indexes[pt]

def erosion_level(pt):
    return (geologic_index(pt) + depth) % 20183
def risk_level(pt):
    return erosion_level(pt) % 3
def region_type(pt):
    return TYPES[risk_level(pt)]

GEARSWITCH_TIME = 7

MOUTH_FASTEST = [7, 0, None]

fastests = {(0, 0): MOUTH_FASTEST}
def update_nearby(pt):
    if pt not in fastests:
        raise RuntimeError("Tried to update nearby when pt had no val")

    updated_pts = set()
    update_vals = set()

    nearby_offsets = [(-1, 0), (0, -1), (1, 0), (0, 1)]
    nearby = ((x + pt[0], y + pt[1]) for (x, y) in nearby_offsets)
    nearby_filt_fst = [(x, y) for (x, y) in nearby if 0 <= x and 0 <= y]
    nearby_filt_snd = [(x, y) for (x, y) in nearby_filt_fst
            if x <= 2000 and y <= 2000]
    if nearby_filt_fst != nearby_filt_snd:
        print("Filtered too large out...")

    nearby_filt = nearby_filt_snd
    
    for near_pt in nearby_filt:
        if region_type(near_pt) == region_type(pt):
            if near_pt not in fastests:
                pt_get_to = [None if (j == INVALID_EQUIP[region_type(pt)])
                        else (fastests[pt][j] + 1)
                        for j in range(len(GEAR))]
                fastests[near_pt] = pt_get_to
                updated_pts.add(near_pt)
                update_vals.update(pt_get_to)
            else:
                for j in range(len(GEAR)):
                    if j == INVALID_EQUIP[region_type(pt)]:
                        continue
                    if fastests[near_pt][j] + 1 < fastests[pt][j]:
                        fastests[pt][j] = fastests[near_pt][j] + 1
                        updated_pts.add(pt)
                        update_vals.add(fastests[pt][j])
                    elif fastests[pt][j] + 1 < fastests[near_pt][j]:
                        fastests[near_pt][j] = fastests[pt][j] + 1
                        updated_pts.add(near_pt)
                        update_vals.add(fastests[near_pt][j])
        else:
            pos_gear = [0, 1, 2]
            my_valid_his_invalid = INVALID_EQUIP[region_type(near_pt)]
            his_valid_my_invalid = INVALID_EQUIP[region_type(pt)]
            pos_gear.remove(my_valid_his_invalid)
            pos_gear.remove(his_valid_my_invalid)
            in_common = pos_gear[0]

            if near_pt not in fastests:
                pt_get_to = [None, None, None]
                pt_get_to[in_common] = fastests[pt][in_common] + 1
                pt_get_to[his_valid_my_invalid] = \
                        fastests[pt][in_common]+GEARSWITCH_TIME + 1
                fastests[near_pt] = pt_get_to
                updated_pts.add(near_pt)
                update_vals.update(pt_get_to)

            else:
                if fastests[near_pt][in_common] + 1 < fastests[pt][in_common]:
                    fastests[pt][in_common] = fastests[near_pt][in_common] + 1
                    updated_pts.add(pt)
                    update_vals.add(fastests[pt][in_common])
                elif fastests[pt][in_common] + 1 < fastests[near_pt][in_common]:
                    fastests[near_pt][in_common] = fastests[pt][in_common] + 1
                    updated_pts.add(near_pt)
                    update_vals.add(fastests[near_pt][in_common])

                if fastests[near_pt][in_common] + GEARSWITCH_TIME + 1 < \
                        fastests[pt][my_valid_his_invalid]:
                    fastests[pt][my_valid_his_invalid] = \
                            fastests[near_pt][in_common] + GEARSWITCH_TIME + 1
                    updated_pts.add(pt)
                    update_vals.add(fastests[pt][my_valid_his_invalid])
                if fastests[pt][in_common] + GEARSWITCH_TIME + 1 < \
                        fastests[near_pt][his_valid_my_invalid]:
                    fastests[near_pt][his_valid_my_invalid] = \
                            fastests[pt][in_common] + GEARSWITCH_TIME + 1
                    updated_pts.add(near_pt)
                    update_vals.add(fastests[near_pt][his_valid_my_invalid])

    return updated_pts, update_vals


def soln_not_found(update_vals):
    if target not in fastests:
        return True

    min_frontier = min(i for i in update_vals if i is not None)
    return min_frontier <= fastests[target][GEAR[TORCH]]

def sum_risk_levels(st, ed):
    return sum(
            sum(risk_level((x, y))
                for x in range(st[0], ed[0]+1)
                )
            for y in range(st[1], ed[1]+1)
            )

if __name__ == "__main__":
    """
    test_pts = [(0,0), (1, 0), (0, 1), (1, 1), (10, 10)]
    for pt in test_pts:
        print(pt, geologic_index(pt), erosion_level(pt), region_type(pt))
    """

    print("Part 1: {}".format(sum_risk_levels((0, 0), target)))

    updated_pts = set([(0,0)])
    update_vals = set([0, 7])
    while soln_not_found(update_vals) and updated_pts:
        new_updated_pts = set()
        new_update_vals = set()
        for pt in updated_pts:
            new_pts, new_vals = update_nearby(pt)
            new_updated_pts.update(new_pts)
            new_update_vals.update(new_vals)

        #print("Updated: {}".format(new_updated_pts))
        updated_pts = new_updated_pts
        update_vals = new_update_vals

    """
    print("Climbing(not |) Torch(not =) Neither(not .)")
    for y in range(5):
        for x in range(5):
            print(fastests[(x,y)], end="")
        print("\n")
    print(fastests[target])
    """

    print("Part 2: {}".format(fastests[target][GEAR[TORCH]]))
