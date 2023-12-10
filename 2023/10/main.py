#!/usr/bin/env python3

class PipeSeg:
    def __init__(self, pt, shape):
        self.pt = pt
        self.shape = shape
    def next_pts(self):
        x, y = self.pt
        mapping = {
            "|": ((x, y - 1), (x, y + 1)),
            "-": ((x - 1, y), (x + 1, y)),
            "L": ((x, y - 1), (x + 1, y)),
            "J": ((x - 1, y), (x, y - 1)),
            "F": ((x, y + 1), (x + 1, y)),
            "7": ((x - 1, y), (x, y + 1)),
        }
        if self.shape == "S":
            raise RuntimeError("asked next pts on an S")
        if self.shape == ".":
            raise RuntimeError("asked next pts on a .")
        else:
            return mapping[self.shape]

class PipeMap:
    def __init__(self):
        self.start_pt = None
        self.max_x = 0
        self.max_y = 0
        self.pipes = dict()

    def add_pipe(self, pt, shape):
        if shape == "S":
            self.start_pt = pt
        self.pipes[pt] = PipeSeg(pt, shape)
        #self.max_x = max(pt[0], self.max_x)
        #self.max_y = max(pt[1], self.max_y)

    def replace_st_pt(self):
        cur_pt = self.start_pt
        poss = [
            ((-1, 0), "-FL"),
            ((1, 0),  "-7J"),
            ((0, -1), "|F7"),
            ((0, 1),  "|JL")
        ]
        result = list()
        for (x_off, y_off), poss_shapes in poss:
            off_pos = (cur_pt[0] + x_off, cur_pt[1] + y_off)
            try:
                if self.pipes[off_pos].shape in poss_shapes:
                    result.append((x_off, y_off))
            except KeyError:
                pass
        if len(result) != 2:
            raise RuntimeError("Not all start connections found")
        
        new_shape = None
        if (-1, 0) in result:
            if (1, 0) in result:
                new_shape = "-"
            elif (0, -1) in result:
                new_shape = "J"
            elif (0, 1) in result:
                new_shape = "7"
            else:
                raise RuntimeError("Difficulty in repl start 0")
        elif (1, 0) in result:
            if (0, -1) in result:
                new_shape = "L"
            elif (0, 1) in result:
                new_shape = "F"
            else:
                raise RuntimeError("Difficulty in repl start 1")
        elif (0, -1) in result:
            if (0, 1) in result:
                new_shape = "|"
            else:
                raise RuntimeError("Difficulty in repl start 2")
        self.pipes[self.start_pt].shape = new_shape

    def next_pts(self, cur_pt):
        """
        if cur_pt == self.start_pt:
            poss = [
                ((-1, 0), "-FL"),
                ((1, 0),  "-7J"),
                ((0, -1), "|F7"),
                ((0, 1),  "|JL")
            ]
            result = list()
            for (x_off, y_off), poss_shapes in poss:
                off_pos = (cur_pt[0] + x_off, cur_pt[1] + y_off)
                try:
                    if self.pipes[off_pos].shape in poss_shapes:
                        result.append(off_pos)
                except KeyError:
                    pass
            if len(result) != 2:
                raise RuntimeError("Not all start connections found")
            return tuple(result)
        """
        return self.pipes[cur_pt].next_pts()

    def next_pt(self, cur_pt, prev_pt):
        poss = self.next_pts(cur_pt)
        return poss[1] if poss[0] == prev_pt else poss[0]

        

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    pm = PipeMap()
    for y, line in enumerate(lines):
        for x, shape in enumerate(line):
            pm.add_pipe((x, y), shape)
    
    pm.replace_st_pt()

    pipe_pts = [pm.start_pt]
    prev_pt = pm.start_pt
    cur_pt = pm.next_pts(pm.start_pt)[0]
    while cur_pt != pm.start_pt:
        pipe_pts.append(cur_pt)
        next_pt = pm.next_pt(cur_pt, prev_pt)
        prev_pt = cur_pt
        cur_pt = next_pt
    print(f"Part 1: {int(len(pipe_pts)/2)}")

    enclosed_cnt = 0
    memoize = dict()
    for y, _line in enumerate(lines):
        #print(y)
        for x, _ in enumerate(_line):
            if (x, y) in pipe_pts:
                continue

            lines_crossed = 0
            cur_pt = (x, y - 1)
            prev_turn = None
            while cur_pt[1] >= 0:
                if cur_pt in memoize:
                    lines_crossed += memoize[cur_pt]
                    break
                if cur_pt in pipe_pts and pm.pipes[cur_pt].shape != "|":
                    if pm.pipes[cur_pt].shape in "LJ":
                        prev_turn = pm.pipes[cur_pt].shape
                    elif pm.pipes[cur_pt].shape == "F":
                        if prev_turn == "J":
                            lines_crossed += 1
                        elif prev_turn == "L":
                            pass
                        else:
                            raise RuntimeError("Unexpected turns F")
                    elif pm.pipes[cur_pt].shape == "7":
                        if prev_turn == "L":
                            lines_crossed += 1
                        elif prev_turn == "J":
                            pass
                        else:
                            raise RuntimeError("Unexpected turns 7")
                    else:
                        lines_crossed += 1
                cur_pt = (cur_pt[0], cur_pt[1] - 1)
            #print((x, y), lines_crossed)
            memoize[(x, y)] = lines_crossed

            is_enclosed = lines_crossed % 2 == 1
            #print(x, y, is_enclosed)
            enclosed_cnt += 1 if is_enclosed else 0
    print(filename, enclosed_cnt)
    
if __name__ == "__main__":
    main("samp01")
    main("samp02")
    main("samp03")
    main("samp04")
    main("input")