#!/usr/bin/env python3

import re

# Next turn defines...
LEFT = "left"
STRT = "straight"
RIGH = "right"

def next_turn_dir(cur_turn_dir):
    move = {LEFT: STRT, STRT: RIGH, RIGH: LEFT}
    return move[cur_turn_dir]

def find_carts(trackmap):
    for y in range(len(trackmap)):
        for x in range(len(trackmap[0])):
            if trackmap[y][x] in ["<", "v", "^", ">"]:
                yield (x,y,trackmap[y][x], LEFT)

def patch_map(trackmap, x, y):
    if (x, y) == (0,0) or (x+1, y+1) == (len(trackmap[0]), len(trackmap)):
        return "/"
    if (x, y+1) == (0,len(trackmap)) or (x+1, y) == (len(trackmap[0]), 0):
        return "\\"
    if x-1 < 0 or x+1 >= len(trackmap[0]):
        return "|"
    if y-1 < 0 or y+1 >= len(trackmap):
        return "-"
    surround_coords = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    surroundings = "".join(trackmap[y][x] for (x,y) in surround_coords)
    if re.match("..[ -][ -]", surroundings):
        return "-"
    elif re.match("[| ][| ]..", surroundings):
        return "|"
    elif re.match("[^ ][^ ][^ ][^ ]", surroundings):
        return "+"
    else:
        raise RuntimeError("Patchmap fail {} {}".format(surroundings, (x,y)))

def print_map(trackmap, carts):
    cart_dict = {(x,y): orient for (x,y,orient,_) in carts}
    for y in range(len(trackmap)):
        for x in range(len(trackmap[0])):
            if (x,y) in cart_dict:
                print(cart_dict[(x,y)], end="")
            else:
                print(trackmap[y][x], end="")

def move_cart(trackmap, cart):
    x, y, orient, nextturn = cart
    orientlookup = {"<": (-1, 0), ">": (1, 0), "^": (0, -1), "v": (0, 1)}
    orientdirx, orientdiry = orientlookup[orient]
    newx, newy = orientdirx + x, orientdiry + y

    # See if we need to turn and do the turn
    neworient = orient
    newnextturn = nextturn
    if trackmap[newy][newx] == "\\":
        swap = {"<": "^", ">": "v", "^": "<", "v": ">"}
        neworient = swap[orient]
    elif trackmap[newy][newx] == "/":
        swap = {"<": "v", ">": "^", "^": ">", "v": "<"}
        neworient = swap[orient]
    elif trackmap[newy][newx] == "+":
        newnextturn = next_turn_dir(nextturn)
        if nextturn == LEFT:
            swap = {"<": "v", ">": "^", "^": "<", "v": ">"}
            neworient = swap[orient]
        elif nextturn == RIGH:
            swap = {"<": "^", ">": "v", "^": ">", "v": "<"}
            neworient = swap[orient]
        elif nextturn == STRT:
            pass
        else:
            raise RuntimeError("Bad nextturn {}".format(nextturn))
    return (newx, newy, neworient, newnextturn)


def move_carts(trackmap, carts):
    # Sort with earlier ys in preference to xs, but then by xs
    carts_sorted = sorted(carts, key=lambda x:(x[1],x[0]))
    new_carts = list()
    to_skip = list()
    for ind, cart in enumerate(carts_sorted):
        if cart in to_skip:
            continue
        new_cart = move_cart(trackmap, cart)

        crashed = False
        for check in carts_sorted[ind+1:]:
            if check in to_skip:
                continue
            if (check[0],check[1]) == (new_cart[0],new_cart[1]):
                crashed = True
                to_skip.append(check)
                print("crash {} rem {}".format((check[0],check[1]), len(carts_sorted)))
        if crashed:
            continue

        for check in new_carts:
            if (check[0],check[1]) == (new_cart[0],new_cart[1]):
                crashed = True
                new_carts.remove(check)
                print("crash {} rem {}".format((check[0],check[1]), len(carts_sorted)))
        if not crashed:
            new_carts.append(new_cart)
    
    return new_carts

    new_carts = [move_cart(trackmap, cart) for cart in carts_sorted]
    cart_set = set()
    # Find colisions
    for (x,y, _, _) in new_carts:
        if (x,y) in cart_set:
            new_carts = [new_cart for new_cart in new_carts 
                    if (new_cart[0], new_cart[1]) != (x,y)]
            print("crash {} rem {}".format((x,y), len(new_carts)))
        else:
            cart_set.add((x,y))
    return new_carts


if __name__ == "__main__":
    with open("input day 13.txt") as f:
        lines = [line for line in f]
    trackmap = [list(line) for line in lines]

    carts = [cart for cart in find_carts(trackmap)]
    for x, y, _, _ in carts:
        trackmap[y][x] = patch_map(trackmap, x, y)
    print_map(trackmap, [])
    print(carts)

    while len(carts) > 1:
        carts = move_carts(trackmap, carts)

    print(carts)

    #print_map(trackmap, carts)
