#!/usr/bin/env python3

def gen_groups():
    lines = (line.strip() for line in open("input.txt", "r").readlines())
    cur_group = list()
    for line in lines:
        if line == "":
            yield cur_group
            cur_group = list()
        else:
            cur_group.append(line)
    yield cur_group

def collapse_group(group):
    return set("".join(group))

def find_union(group):
    other = [set(a) for a in group[1:]]
    return set(group[0]).intersection(*other)

if __name__ == "__main__":
    groups = list(gen_groups())
    collapsed = [collapse_group(group) for group in groups]
    
    print("Part 1: {}".format(sum(len(g) for g in collapsed)))

    unioned = [find_union(group) for group in groups]
    print("Part 2: {}".format(sum(len(g) for g in unioned)))
