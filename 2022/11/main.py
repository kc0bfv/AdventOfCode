#!/usr/bin/env python3

import functools
import math
import operator as op

class Monkey:
    def __init__(self, line_iter):
        mon, numcolon = next(line_iter).split()
        assert(mon == "Monkey")
        num, _ = numcolon.split(":")
        self.num = int(num)

        _, itemlist = next(line_iter).split(":")
        self.items = [int(item.strip()) for item in itemlist.split(",")]

        _, opcmd = next(line_iter).split("=")
        self.op_first, op_oper, self.op_second = opcmd.strip().split(" ")
        self.op_oper = op.mul if op_oper == "*" else op.add

        self.div_by = int(next(line_iter).split(" ")[-1])
        self.test_true = int(next(line_iter).split(" ")[-1])
        self.test_false = int(next(line_iter).split(" ")[-1])

        self.inspect_count = 0

    def __str__(self):
        return f"{self.num} {self.items} {self.inspect_count} {self.op_first} {self.op_oper} {self.op_second} {self.div_by} {self.test_true} {self.test_false}"

    def run_op(self, item, mod):
        second = item if self.op_second == "old" else int(self.op_second)
        return self.op_oper(item, second) % mod

    def do_test(self, worry):
        return 0 == worry % self.div_by

    def get_dest(self, test_result):
        return self.test_true if test_result else self.test_false

def prod(iterable):
    return functools.reduce(op.mul, iterable, 1)

def do_round(monkeys, mod, worry_abates = True):
    doprint = worry_abates
    for monkey in monkeys:
        if doprint:
            print(f"Monkey {monkey.num}")
        items, monkey.items = monkey.items, list()
        for item in items:
            monkey.inspect_count += 1
            worry_orig = monkey.run_op(item, mod)
            if worry_abates:
                worry = math.floor(worry_orig / 3)
            else:
                worry = worry_orig
            test = monkey.do_test(worry)
            dest = monkey.get_dest(test)
            monkeys[dest].items.append(worry)
            if doprint:
                print(f"Inspects {item}")
                print(f"Worry level rises to {worry_orig}")
                if worry_abates:
                    print(f"Worry drops to {worry}")
                print(f"Test result {test}")
                print(f"Item worry {worry} thrown to {dest}")
                print("")

def read_monkeys(lines):
    monkeys = list()
    lineiter = iter(lines)
    try:
        while True:
            monkeys.append(Monkey(lineiter))
            next(lineiter)
    except StopIteration:
        pass
    return monkeys

def main(filename):
    with open(filename, "r") as f:
        lines = [l.strip() for l in f]

    monkeys = read_monkeys(lines)

    mod = prod(monkey.div_by for monkey in monkeys)

    for rnd in range(20):
        print(f"Round {rnd}")
        do_round(monkeys, mod)
        for monkey in monkeys:
            print(monkey)

    monkey_bus = [monkey.inspect_count for monkey in monkeys]
    p1 = prod(sorted(monkey_bus)[-2:])
    print(f"Part 1: {p1}")

    monkeys = read_monkeys(lines)
    for rnd in range(10000):
        do_round(monkeys, mod, False)

    monkey_bus = [monkey.inspect_count for monkey in monkeys]
    p2 = prod(sorted(monkey_bus)[-2:])
    print(f"Part 2: {p2}")

if __name__ == "__main__":
    main("samp")
    main("input.txt")
