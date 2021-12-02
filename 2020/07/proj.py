#!/usr/bin/env python3

from collections import defaultdict

def yield_contains(cont_list):
    if cont_list == "no other bags":
        return
    for line_r in cont_list.split(","):
        line = line_r.strip()
        sp0_ind = line.index(" ")
        bag_ind = line.rindex("bag")

        num = int(line[:sp0_ind])
        color = line[sp0_ind:bag_ind].strip()
        yield (num, color)

def yield_node(filename):
    lines = (l.strip() for l in open(filename, "r").readlines())
    for line in lines:
        bag0_ind = line.index("bag", 0)
        contains_ind = line.index("contain", bag0_ind) + len("contain ")

        base_color = line[:bag0_ind].strip()
        contains = list(yield_contains(line[contains_ind:-1]))

        cont_colors = set(c[1] for c in contains)

        yield (base_color, contains, cont_colors)

def yield_cont(color, nodes):
    for (bc, _, cc) in nodes:
        if color in cc:
            yield bc

def expand_level(level_dict, nodes_d):
    level_result = defaultdict(int)
    for color, count in level_dict.items():
        conts = nodes_d[color]
        for c_n, c_c in conts:
            level_result[c_c] += count * c_n
    return level_result

if __name__ == "__main__":
    nodes = list(yield_node("input.txt"))

    nodes_dict = {n[0]: n[1] for n in nodes}

    conting = set()
    new_conting = set(yield_cont("shiny gold", nodes))
    while len(new_conting) > len(conting):
        conting = set(new_conting)
        for color in conting:
            new_conting.update(yield_cont(color, nodes))

    print("Part 1: {}".format(len(new_conting)))

    cur_lev = {"shiny gold": 1}
    new_lev = expand_level(cur_lev, nodes_dict)
    bag_count = 0
    while len(new_lev) > 0:
        bag_count += sum(c for _,c in new_lev.items())
        cur_lev = new_lev
        new_lev = expand_level(cur_lev, nodes_dict)

    print("Part 2: {}".format(bag_count))
