#!/usr/bin/env python3

import functools
import operator
import json

COLORS = ["red", "green", "blue"]

def product(it):
    return functools.reduce(operator.mul, it, 1)

class Pull:
    def __init__(self, pull):
        balls = (b.strip().split(" ") for b in pull.split(","))
        self.balls = {color: 0 for color in COLORS}
        for cnt, color in balls:
            if color not in self.balls.keys():
                raise RuntimeError(f"Invalid color: {color} {pull}")
            if self.balls[color] != 0:
                raise RuntimeError(f"Color seen twice: {color} {pull}")
            self.balls[color] = int(cnt)

    def as_xyz(self):
        return {"x": self.balls["red"], "y": self.balls["green"], "z": self.balls["blue"]}

class Game:
    def __init__(self, line):
        self.gameno = int(line.split(":")[0][5:].strip())
        self.pulls = [Pull(pull.strip()) for pull in line.split(":")[1].strip().split(";")]

    def color_mins(self):
        retval = {color: 0 for color in COLORS}
        for pull in self.pulls:
            for color in COLORS:
                if pull.balls[color] > retval[color]:
                    retval[color] = pull.balls[color]
        return retval

    def p1_possible(self):
        mins = self.color_mins()
        return mins["red"] <= 12 and mins["green"] <= 13 and mins["blue"] <= 14

    def as_xyz(self):
        pull_xyz = [pull.as_xyz() for pull in self.pulls]
        return {key: [pull[key] for pull in pull_xyz] for key in ["x", "y", "z"]}

def to_plotly_trace(xyz):
    xyz["mode"] = "lines"
    xyz["type"] = "scatter3d"
    return xyz

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    games = [Game(line) for line in lines]
    p1_games = [game.gameno for game in games if game.p1_possible()]
    print(f"P1: {sum(p1_games)}")
    p2_powers = [product(game.color_mins().values()) for game in games]
    print(f"P2: {sum(p2_powers)}")

    game_xyz = [to_plotly_trace(game.as_xyz()) for game in games]
    print(json.dumps(game_xyz))

if __name__ == "__main__":
    main("samp1")
    main("input")