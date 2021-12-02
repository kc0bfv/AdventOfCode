#!/usr/bin/env python2

import re

def read(filename):
    inputs = list()
    with open(filename, "r") as fin:
        for line in fin:
            mtch = re.match("([0-9]+)-([0-9]+) ([a-zA-Z]): ([a-zA-Z]+)", line)
            inputs.append(mtch.groups())
    return inputs

def find_wrong(inputs):
    correct = 0
    for min_c, max_c, val, pw in inputs:
        cnt = pw.count(val)
        if int(min_c) > cnt or int(max_c) < cnt:
            print(min_c, max_c, val, pw)
        else:
            correct += 1
    return correct

def find_wrong2(inputs):
    correct = 0
    for ind1_c, ind2_c, val, pw in inputs:
        ind_1, ind_2 = int(ind1_c) - 1, int(ind2_c) - 1
        if (pw[ind_1] == val) == (pw[ind_2] == val):
            print(ind1_c, ind2_c, val, pw)
        else:
            correct += 1
    return correct

if __name__ == "__main__":
    inputs = read("input.txt")
    print(find_wrong(inputs))
    print(find_wrong2(inputs))

