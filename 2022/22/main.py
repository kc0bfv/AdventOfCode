#!/usr/bin/env python3

from enum import Enum

ARROW_LOOKUP = {0: ">", 1: "V", 2: "<", 3: "^"}

class Move:
    def __init__(self, dist, direc):
        assert(isinstance(dist, int))
        assert(direc == "R" or direc == "L" or direc == None)
        self.dist = dist
        self.direc = direc

class Space(Enum):
    Empty = 0
    Valid = 1
    Wall = 2

    @classmethod
    def resolve(cls, char):
        res = {" ": cls.Empty, ".": cls.Valid, "#": cls.Wall}
        return res[char]

    def __str__(self):
        if self is Space.Empty:
            return " "
        elif self is Space.Valid:
            return "."
        elif self is Space.Wall:
            return "#"

class Board:
    def __init__(self, lines, mode, cubeshape):
        st_pos = {
            0: (8, 0, 0),
            1: (50, 0, 0)
        }

        self.cubeshape = cubeshape
        self.mode = mode
        self.height = len(lines)
        self.width = max(len(line) for line in lines)
        filled_lines = [line + " " * (self.width - len(line)) for line in lines]
        self.board = [[Space.resolve(char) for char in line] for line in filled_lines]
        self.cur_pos = st_pos[cubeshape]
        self.memo_next = dict()
        self.past_locs = {self.cur_pos[:2]: ARROW_LOOKUP[self.cur_pos[2]]}

        self.cube_wrap = self.cube_0_moves() if self.cubeshape == 0 else self.cube_1_moves()

    def make_move(self, move):
        self.move_dist(move.dist)
        self.turn(move.direc)

    def turn(self, direc):
        if direc is None:
            return
        direc_int = 1 if direc == "R" else -1
        self.cur_pos = (self.cur_pos[0], self.cur_pos[1], (self.cur_pos[2] + direc_int) % 4)
        self.past_locs[self.cur_pos[:2]] = ARROW_LOOKUP[self.cur_pos[2]]

    def move_dist(self, dist):
        for ind in range(dist):
            next_pos = self.find_next_flat() if self.mode == "FLAT" else self.find_next_cube()
            at_next_pos = self.board[next_pos[1]][next_pos[0]]
            if at_next_pos is Space.Empty:
                raise RuntimeError("Next pos is empty!")
            elif at_next_pos is Space.Wall:
                return
            elif at_next_pos is Space.Valid:
                self.cur_pos = next_pos
            else:
                raise RuntimeError(f"Next pos is wonky: {next_pos} {at_next_pos}")

            self.past_locs[self.cur_pos[:2]] = ARROW_LOOKUP[self.cur_pos[2]]
    
    def find_next_flat(self):
        if self.cur_pos in self.memo_next:
            return self.memo_next[self.cur_pos]
        dir_add_dict = {
            0: (1, 0),
            1: (0, 1),
            2: (-1, 0),
            3: (0, -1),
        }
        dir_add = dir_add_dict[self.cur_pos[2]]

        test_pos = ((self.cur_pos[0] + dir_add[0]) % self.width,
                (self.cur_pos[1] + dir_add[1]) % self.height,
                self.cur_pos[2])

        while self.board[test_pos[1]][test_pos[0]] is Space.Empty:
            test_pos = ((test_pos[0] + dir_add[0]) % self.width,
                    (test_pos[1] + dir_add[1]) % self.height,
                    test_pos[2])

        self.memo_next[self.cur_pos] = test_pos
        return test_pos

    def find_next_cube(self):
        dir_add_dict = {
            0: (1, 0),
            1: (0, 1),
            2: (-1, 0),
            3: (0, -1),
        }
        dir_add = dir_add_dict[self.cur_pos[2]]

        test_pos = ((self.cur_pos[0] + dir_add[0]),
                (self.cur_pos[1] + dir_add[1]),
                self.cur_pos[2])

        try:
            if self.board[test_pos[1]][test_pos[0]] is not Space.Empty:
                return test_pos
        except IndexError:
            pass

        return self.cube_wrap[test_pos]

    def gen_edge(self, edge1, edge2):
        # for two cube edges correlating, generate the next moves
        retval = dict()

        edge1_p1, edge1_p2, edge1_dir = edge1
        edge2_p1, edge2_p2, edge2_dir = edge2

        edge1_xdiff = edge1_p2[0] - edge1_p1[0]
        edge1_ydiff = edge1_p2[1] - edge1_p1[1]
        # Make sure one dimension changes and the other doesn't
        assert(
            (edge1_xdiff != 0 and edge1_ydiff == 0) or 
            (edge1_ydiff != 0 and edge1_xdiff == 0))
        edge1_move_axis = 0 if edge1_xdiff != 0 else 1
        edge1_stat_axis = 0 if edge1_xdiff == 0 else 1
        edge1_move_diff = edge1_xdiff if edge1_xdiff != 0 else edge1_ydiff
        edge1_range_step = 1 if edge1_move_diff > 0 else -1
        edge1_range_st = edge1_p1[edge1_move_axis]
        edge1_range_ed = edge1_p2[edge1_move_axis] + edge1_range_step
        edge1_move_off_dir = 1 if edge1_dir in [0, 1] else -1

        edge2_xdiff = edge2_p2[0] - edge2_p1[0]
        edge2_ydiff = edge2_p2[1] - edge2_p1[1]
        # Make sure one dimension changes and the other doesn't
        assert(
            (edge2_xdiff != 0 and edge2_ydiff == 0) or 
            (edge2_ydiff != 0 and edge2_xdiff == 0))
        edge2_move_axis = 0 if edge2_xdiff != 0 else 1
        edge2_stat_axis = 0 if edge2_xdiff == 0 else 1
        edge2_move_diff = edge2_xdiff if edge2_xdiff != 0 else edge2_ydiff
        edge2_range_step = 1 if edge2_move_diff > 0 else -1
        edge2_range_st = edge2_p1[edge2_move_axis]
        edge2_range_ed = edge2_p2[edge2_move_axis] + edge2_range_step
        # this one is flipped because we specified edge2_dir in terms of how rolling
        # off edge1 lands on edge2, and move off happens in the opposite direction
        edge2_move_off_dir = -1 if edge2_dir in [0, 1] else 1

        # Make sure the edges are the same length
        assert(max([abs(edge1_xdiff), abs(edge1_ydiff)]) == max([abs(edge2_xdiff), abs(edge2_ydiff)]))

        for i1, i2 in zip(range(edge1_range_st, edge1_range_ed, edge1_range_step),
                range(edge2_range_st, edge2_range_ed, edge2_range_step)):

            # For rolling off edge 1
            res_edge1 = [0, 0, edge1_dir]
            res_edge1[edge1_move_axis] = i1
            res_edge1[edge1_stat_axis] = edge1_p1[edge1_stat_axis] + edge1_move_off_dir

            res_edge2 = [0, 0, edge2_dir]
            res_edge2[edge2_move_axis] = i2
            res_edge2[edge2_stat_axis] = edge2_p1[edge2_stat_axis]

            assert(tuple(res_edge1) not in retval)
            retval[tuple(res_edge1)] = tuple(res_edge2)

            # For rolling off edge 2
            res_edge2 = [0, 0, (edge2_dir + 2) % 4]
            res_edge2[edge2_move_axis] = i2
            res_edge2[edge2_stat_axis] = edge2_p1[edge2_stat_axis] + edge2_move_off_dir

            res_edge1 = [0, 0, (edge1_dir + 2) % 4]
            res_edge1[edge1_move_axis] = i1
            res_edge1[edge1_stat_axis] = edge1_p1[edge1_stat_axis]

            assert(tuple(res_edge2) not in retval)
            retval[tuple(res_edge2)] = tuple(res_edge1)
        return retval

    def cube_1_moves(self):
        dict_pos = dict()

        # (50, 0, 2) - (50, 49, 2) -> (0, 149, 0) - (0, 100, 0)
    
        dict_pos.update(self.gen_edge(
            ((50, 0), (50, 49), 2),
            ((0, 149), (0, 100), 0)
        ))

        # (50, 99, 2) - (50, 50, 2) -> (49, 100, 1) - (0, 100, 1)
    
        dict_pos.update(self.gen_edge(
            ((50, 99), (50, 50), 2),
            ((49, 100), (0, 100), 1)
        ))

        # (50, 0, 3) - (99, 0, 3) -> (0, 150, 0) - (0, 199, 0)
    
        dict_pos.update(self.gen_edge(
            ((50, 0), (99, 0), 3),
            ((0, 150), (0, 199), 0)
        ))

        # (50, 149, 1) - (99, 149, 1) -> (49, 150, 2) - (49, 199, 2)
    
        dict_pos.update(self.gen_edge(
            ((50, 149), (99, 149), 1),
            ((49, 150), (49, 199), 2)
        ))

        # (149, 0, 3) - (100, 0, 3) -> (49, 199, 3) - (0, 199, 3)
    
        dict_pos.update(self.gen_edge(
            ((149, 0), (100, 0), 3),
            ((49, 199), (0, 199), 3)
        ))

        # (100, 49, 1) - (149, 49, 1) -> (99, 50, 2) - (99, 99, 2)
    
        dict_pos.update(self.gen_edge(
            ((100, 49), (149, 49), 1),
            ((99, 50), (99, 99), 2)
        ))

        # (99, 100, 0) - (99, 149, 0) -> (149, 49, 2) - (149, 0, 2)
    
        dict_pos.update(self.gen_edge(
            ((99, 100), (99, 149), 0),
            ((149, 49), (149, 0), 2)
        ))

        return dict_pos


    def cube_0_moves(self):
        # Return a dictionary of mappings from flat next-move to cube next-move
        dict_pos = dict()

        #(8, -1, 3) - (11, -1, 3) -> (3, 4, 1) - (0, 4, 1)
        #(0, 3, 3) - (3, 3, 3) -> (11, 0, 1) - (8, 0, 1)
    
        dict_pos.update(self.gen_edge(
            ((8, 0), (11, 0), 3),
            ((3, 4), (0, 4), 1)
        ))

        #(7, 0, 2) - (7, 3, 2) -> (4, 4, 1) - (7, 4, 1)
        #(4, 3, 3) - (7, 3, 3) -> (8, 0, 0) - (8, 3, 0)

        dict_pos.update(self.gen_edge(
            ((8, 0), (8, 3), 2),
            ((4, 4), (7, 4), 1)
        ))

        #(-1, 4, 2) - (-1, 7, 2) -> (12, 11, 3) - (15, 11, 3)
        #(12, 12, 1) - (15, 12, 1) -> (0, 4, 0) - (0, 7, 0)

        dict_pos.update(self.gen_edge(
            ((0, 4), (0, 7), 2),
            ((12, 11), (15, 11), 3)
        ))

        #(0, 8, 1) - (3, 8, 1) -> (8, 11, 3) - (11, 11, 3)
        #(8, 12, 1) - (11, 12, 1) -> (0, 7, 3) - (3, 7, 3)

        dict_pos.update(self.gen_edge(
            ((0, 7), (3, 7), 1),
            ((11, 11), (8, 11), 3)
        ))

        #(4, 8, 1) - (7, 8, 1) -> (8, 11, 0) - (8, 8, 0)
        #(7, 11, 2) - (7, 8, 2) -> (4, 7, 3) - (7, 7, 3)

        dict_pos.update(self.gen_edge(
            ((4, 7), (7, 7), 1),
            ((8, 11), (8, 8), 0)
        ))

        #(16, 11, 0) - (16, 8, 0) -> (11, 0, 2) - (11, 3, 2)
        #(12, 0, 0) - (12, 3, 0) -> (15, 11, 2) - (15, 8, 2)

        dict_pos.update(self.gen_edge(
            ((15, 11), (15, 8), 0),
            ((11, 0), (11, 3), 2)
        ))

        #(12, 7, 3) - (15, 7, 3) -> (11, 7, 2) - (11, 4, 2)
        #(12, 7, 0) - (12, 4, 0) -> (12, 8, 1) - (15, 8, 1)

        dict_pos.update(self.gen_edge(
            ((12, 8), (15, 8), 3),
            ((11, 7), (11, 4), 2)
        ))

        return dict_pos
        

    def __str__(self):
        return "\n".join(
                "".join(
                    self.past_locs[(col_ind,row_ind)] if (col_ind, row_ind) in self.past_locs else str(space)
                    for col_ind, space in enumerate(row)
                )
            for row_ind, row in enumerate(self.board)
        )
        

def gen_splits(line):
    cur = ""
    for char in line.strip("\n"):
        if char == "R" or char == "L":
            yield (int(cur), char)
            cur = ""
        else:
            cur += char
    if cur != "":
        yield int(cur), None

def main(filename, cubeshape):
    with open(filename, "r") as f:
        lines = [line.strip("\n") for line in f]

    board = Board(lines[:-2], "FLAT", cubeshape)
    moves = list(Move(dist, direc) for dist, direc in gen_splits(lines[-1]))

    for move in moves:
        board.make_move(move)

    p1 = (1000 * (board.cur_pos[1] + 1)) + (4 * (board.cur_pos[0]+1)) + board.cur_pos[2]
    print(f"Part 1: {p1}")

    board = Board(lines[:-2], "CUBE", cubeshape)

    for move in moves:
        board.make_move(move)


    p2 = (1000 * (board.cur_pos[1] + 1)) + (4 * (board.cur_pos[0]+1)) + board.cur_pos[2]
    print(f"Part 2: {p2}")

if __name__ == "__main__":
    main("samp", 0)
    main("input.txt", 1)
