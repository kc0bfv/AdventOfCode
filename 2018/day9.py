#!/usr/bin/env python3

class DblLinkLoopNod:
    def __init__(self, val, nxt, prv):
        self.val = val
        self.nxt = nxt if nxt else self
        self.prv = prv if prv else self

class DblLinkLoop:
    def __init__(self):
        self.cur = None

    def insert_after_cur(self, val):
        if self.cur is None:
            self.cur = DblLinkLoopNod(val, None, None)
        else:
            self.cur = DblLinkLoopNod(val, self.cur.nxt, self.cur)
            self.cur.nxt.prv = self.cur
            self.cur.prv.nxt = self.cur

    def get_cur_val(self):
        return self.cur.val

    def del_cur(self):
        tmp = self.cur
        val = tmp.val
        self.cur.prv.nxt = self.cur.nxt
        self.cur.nxt.prv = self.cur.prv
        self.cur = self.cur.nxt
        del tmp
        return val

    def move_nxt(self, cnt):
        if self.cur is None:
            raise RuntimeError("No values in loop")
        else:
            for _ in range(cnt):
                self.cur = self.cur.nxt

    def move_prv(self, cnt):
        if self.cur is None:
            raise RuntimeError("No values in loop")
        else:
            for _ in range(cnt):
                self.cur = self.cur.prv

if __name__ == "__main__":
    #player_cnt = 458
    #marble_cnt = 72019*100
    player_cnt = 9
    marble_cnt = 400

    player_scores = [0]*player_cnt
    marble_circle = DblLinkLoop()

    cur_player = 0

    for i in range(marble_cnt+1):
        #if i % 10000 == 0:
        #    print(i)
        if i != 0 and i % 23 == 0:
            player_scores[cur_player] += i
            marble_circle.move_prv(7)
            player_scores[cur_player] += marble_circle.get_cur_val()
            marble_circle.del_cur()
        else:
            if i != 0:
                marble_circle.move_nxt(1)
            marble_circle.insert_after_cur(i)

        cur_player += 1
        cur_player %= player_cnt

    print(player_scores)
    print(max(player_scores))
