#!/usr/bin/env python3

import re

class Part(dict):
    def __init__(self, line):
        patt = "\{x=(?P<x>[0-9]+),m=(?P<m>[0-9]+),a=(?P<a>[0-9]+),s=(?P<s>[0-9]+)\}"
        match = re.match(patt, line)
        self.x, self.m, self.a, self.s = [int(v) for v in match.groups()]
        self["x"], self["m"], self["a"], self["s"] = self.x, self.m, self.a, self.s

class Rule:
    def __init__(self, rule_str):
        if ":" not in rule_str:
            self.test_func = lambda part: True
            self.dest = rule_str
        else:
            colon_pt = rule_str.find(":")
            self.dest = rule_str[colon_pt+1:]
            cond_part = rule_str[:colon_pt]
            int_part = int(cond_part[2:])
            let_part = cond_part[0]
            if cond_part[1] == "<":
                self.test_func = lambda part, i=int_part, l=let_part: part[l] < int_part
            elif cond_part[1] == ">":
                self.test_func = lambda part, i=int_part, l=let_part: part[l] > int_part
            else:
                raise RuntimeError("Didn't find cond part")
                
    def run(self, part):
        if self.test_func(part):
            return self.dest
        return None

class Flow:
    def __init__(self, line):
        paren = line.find("{")
        self.name = line[:paren]
        patt = line[paren+1:-1]
        self.rules = [Rule(part) for part in patt.split(",")]

    def run(self, part):
        for rule in self.rules:
            out = rule.run(part)
            if out is not None:
                return out
        raise RuntimeError("Hit end of flow!")

def run_flows(part, flows):
    cur_flow = "in"
    while cur_flow not in ["A", "R"]:
        cur_flow = flows[cur_flow].run(part)
    return cur_flow

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    blank_ln = [num for num, line in enumerate(lines) if line == ""][0]
    flow_lines = lines[:blank_ln]
    part_lines = lines[blank_ln+1:]

    flows_raw = (Flow(line) for line in flow_lines)
    flows = {flow.name: flow for flow in flows_raw}
    parts = [Part(line) for line in part_lines]

    parts_run = [run_flows(part, flows) for part in parts]
    part1 = sum(sum(part.values()) for part, result in zip(parts, parts_run) if result == "A")
    print(f"Part 1: {part1}")

if __name__ == "__main__":
    main("samp01")
    main("input")