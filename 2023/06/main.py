#!/usr/bin/env python3

import re
import functools
import operator
import math

def product(it):
    return functools.reduce(operator.mul, it, 1)

"""
t - time held
l - race len
d - dist gone
d = (l-t) * (t)
"""

def get_dist(racelen, timeheld):
    return (racelen - timeheld) * timeheld

def quadratic(a, b, c):
    plusmin = lambda d: (-b + (d * math.sqrt((b*b) - (4 * a * c)))) / (2*a)
    return (plusmin(1), plusmin(-1))

def calc_p1(racelen, mindist):
    midpt = int(racelen / 2)
    is_even = racelen % 2 == 0
    cur_pt = midpt
    
    mid_dist = get_dist(racelen, cur_pt)

    if not is_even:
        cnt = math.ceil(max(quadratic(1, 1, -(mid_dist - mindist))))
    else:
        cnt = math.ceil(math.sqrt(mid_dist-mindist))
    return (cnt * 2) - (1 if is_even else 0)

    # My old, slow way is below
    cnt = 0
    print("mid", get_dist(racelen, cur_pt), mindist)
    if not is_even:
        margin = get_dist(racelen, cur_pt) - mindist
    if is_even:
        print("mid", get_dist(racelen, cur_pt+1), mindist)
    if get_dist(racelen, cur_pt) < mindist:
        return 0
    while cur_pt >= 0:
        cnt += 1
        cur_pt -= 1
        print(cnt, cur_pt, get_dist(racelen, cur_pt))
        if get_dist(racelen, cur_pt) <= mindist:
            break
    return (cnt * 2) - (1 if is_even else 0)

def main(filename):
    with open(filename) as f:
        times, dists = ([int(n) for n in re.split(" +", line)[1:]] for line in f)
    races = list(zip(times, dists))
    winways = [calc_p1(*race) for race in races]
    #print(winways)
    print(f"Part {filename}: {product(winways)}")

if __name__ == "__main__":
    main("samp01")
    main("samp02")
    main("input")
    main("inputp2")