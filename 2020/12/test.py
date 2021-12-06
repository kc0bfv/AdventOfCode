#!/usr/bin/env python3

import math
from sys import argv

def vec_len(n, e):
    return math.sqrt((e ** 2) + (n ** 2))

def vec_deg(n, e):
    if e == 0:
        return 90 if n >= 0 else 270
    elif n == 0:
        return 0 if e >= 0 else 180
    else:
        ang = math.degrees(math.atan(abs(n/e)))
        if n > 0 and e > 0:
            return ang
        elif n > 0 and e < 0:
            return ang + 90
        elif n < 0 and e < 0:
            return ang + 180
        elif n < 0 and e > 0:
            return ang + 270
        else:
            print("ERROR angle {} {} {}".format(n, e, ang))

def vec_n(vecl, vecd):
    return round(vecl * math.sin(math.radians(vecd)))
def vec_e(vecl, vecd):
    return round(vecl * math.cos(math.radians(vecd)))

def trier(n, e):
    print("n ", n, " e ", e)
    print(vec_deg(n, e))
    print(vec_len(n, e))
    print(vec_n(vec_len(n, e), vec_deg(n, e)), vec_e(vec_len(n, e), vec_deg(n, e)))

if __name__ == "__main__":
    trier(0, 0)
    trier(0, 10)
    trier(5, 5)
    trier(10, 0)
    trier(5, -5)
    trier(0, -10)
    trier(-5, -5)
    trier(-10, 0)
    trier(-2, 5)
    trier(-5, 5)
