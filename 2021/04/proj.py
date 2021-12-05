#!/usr/bin/env python3

from sys import argv
from itertools import zip_longest

def grouper(iterable, n, fillvalue=None):
    "Collect data into non-overlapping fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx
    args = [iter(iterable)] * n
    return zip_longest(*args, fillvalue=fillvalue)

class Board:
    def __init__(self, b_in):
        self.board = [[int(i) for i in line.split()] for line in b_in]
        self.marked = [[False for i in line] for line in self.board]
    def __str__(self):
        retval = ""
        for b_r, m_r in zip(self.board, self.marked):
            for b_c, m_c in zip(b_r, m_r):
                if m_c:
                    retval += "*{}* ".format(b_c)
                else:
                    retval += "{} ".format(b_c)
            retval += "\n"
        return retval
    def mark(self, val):
        for row_ind, row in enumerate(self.board):
            for col_ind, col in enumerate(row):
                if col == val:
                    self.marked[row_ind][col_ind] = True
    def check_win(self):
        for row in self.marked:
            if all(row):
                return True
        cols = [[row[ind] for row in self.marked] for ind in range(5)]
        for col in cols:
            if all(col):
                return True
        diag0 = [self.marked[ind][ind] for ind in range(5)]
        diag1 = [self.marked[ind][4-ind] for ind in range(5)]
        if all(diag0) or all(diag1):
            pass
            #return True
        return False
    def board_score(self, mult):
        retval = 0
        for b_r, m_r in zip(self.board, self.marked):
            for b_c, m_c in zip(b_r, m_r):
                if not m_c:
                    retval += b_c
        return retval * mult


if __name__ == "__main__":
    lines = [line.strip() for line in open(argv[1], "r").readlines()]
    nums = [int(val) for val in lines[0].split(",")]
    boards_almost = grouper(lines[1:], 6)
    boards = [Board(board[1:]) for board in boards_almost]

    won_boards = set()
    first_win = False
    next_win_part_2 = False

    for num in nums:
        for board in boards:
            board.mark(num)
        for ind, board in enumerate(boards):
            if board.check_win():
                if not first_win:
                    print("Part 1: {} {} {}".format(ind+1, num, board.board_score(num)))
                    first_win = True

                won_boards.add(board)
            if not next_win_part_2 and (len(boards) <= len(won_boards) + 1):
                next_win_part_2 = True
            elif next_win_part_2 and (len(boards) == len(won_boards)):
                print("Part 2: {}".format(board.board_score(num)))
                next_win_part_2 = None
                break
        if next_win_part_2 is None:
            break
                    
