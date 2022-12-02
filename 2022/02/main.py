#!/usr/bin/env python3

from enum import IntEnum

class Hand(IntEnum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

class Game(IntEnum):
    WIN = 6
    TIE = 3
    LOSS = 0

def parsegame(line):
    other_lookup = {"A": Hand.ROCK, "B": Hand.PAPER, "C": Hand.SCISSORS}
    you_lookup = {"X": Hand.ROCK, "Y": Hand.PAPER, "Z": Hand.SCISSORS}

    val_o, val_u = line.strip().split(" ")

    return (other_lookup[val_o], you_lookup[val_u])

def parsegame_2(line):
    other_lookup = {"A": Hand.ROCK, "B": Hand.PAPER, "C": Hand.SCISSORS}
    result_lookup = {"X": Game.LOSS, "Y": Game.TIE, "Z": Game.WIN}

    val_o, val_u = line.strip().split(" ")

    return (other_lookup[val_o], result_lookup[val_u])

def find_move_2(game2):
    them, result = game2

    # they win if their hand is key and yours is value
    win_cond = {
        Hand.ROCK: Hand.SCISSORS,
        Hand.SCISSORS: Hand.PAPER,
        Hand.PAPER: Hand.ROCK,
    }
    
    if result == Game.TIE:
        return them
    elif result == Game.LOSS:
        return win_cond[them]
    else:
        poss = set([Hand.ROCK, Hand.SCISSORS, Hand.PAPER])
        poss.remove(them)
        poss.remove(win_cond[them])
        if len(poss) > 1:
            assert(False)
        return poss.pop()

def checkwin(game):
    them, you = game

    # you win if your hand is key and theirs is value
    win_cond = {
        Hand.ROCK: Hand.SCISSORS,
        Hand.SCISSORS: Hand.PAPER,
        Hand.PAPER: Hand.ROCK,
    }

    if you == them:
        return Game.TIE
    elif them == win_cond[you]: 
        return Game.WIN
    else:
        return Game.LOSS

def scoregame(game):
    return checkwin(game) + game[1]

def main(filename):
    with open(filename, "r") as f:
        lines = f.readlines()

    games = [parsegame(line) for line in lines]

    scores = [scoregame(game) for game in games]
    sumscore = sum(scores)

    print(f"Total score: {sumscore}")

    games2 = [parsegame_2(line) for line in lines]
    moves2 = [(game[0], find_move_2(game)) for game in games2]
    scores2 = [scoregame(move) for move in moves2]
    sumscore2 = sum(scores2)

    print(f"Total score 2: {sumscore2}")

if __name__ == "__main__":
    main("input.txt")
