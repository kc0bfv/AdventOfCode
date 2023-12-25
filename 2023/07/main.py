#!/usr/bin/env python3

from collections import defaultdict
from functools import total_ordering

HANDTYPES = {
    "FIVE": 6,
    "FOUR": 5,
    "FULL": 4,
    "THREE": 3,
    "TWO": 2,
    "ONE": 1,
    "HIGH": 0,
}
RANK = {val: ind for (ind, val) in enumerate("23456789TJQKA")}
P2RANK = {val: ind for (ind, val) in enumerate("J23456789TQKA")}

@total_ordering
class Hand:
    def __init__(self, line):
        cards, bid = line.split(" ")
        self.bid = int(bid)
        self.cards = cards
    @property
    def handtype(self):
        card_counts = defaultdict(int)
        for card in self.cards:
            card_counts[card] += 1
        card_types = len(card_counts.keys())
        if card_types == 1:
            return "FIVE"
        elif card_types == 2 and 4 in card_counts.values():
            return "FOUR"
        elif card_types == 2 and 3 in card_counts.values():
            return "FULL"
        elif card_types == 3 and 3 in card_counts.values():
            return "THREE"
        elif card_types == 3:
            return "TWO"
        elif card_types == 4:
            return "ONE"
        else:
            return "HIGH"
    @property
    def handtype_val(self):
        return HANDTYPES[self.handtype]
    def __eq__(self, other):
        return self.cards == other.cards
    def __lt__(self, other):
        if self.handtype_val < other.handtype_val:
            return True
        elif self.handtype_val > other.handtype_val:
            return False
        for sc, oc in zip(self.cards, other.cards):
            if sc == oc:
                continue
            return RANK[sc] < RANK[oc]
    def __repr__(self):
        return f"{self.cards} {self.handtype} {self.bid}"

@total_ordering
class P2Hand:
    def __init__(self, line):
        cards, bid = line.split(" ")
        self.bid = int(bid)
        self.cards = cards
    @property
    def handtype(self):
        card_counts = defaultdict(int)
        for card in self.cards:
            card_counts[card] += 1
        if "J" in card_counts.keys() and len(card_counts.keys()) > 1:
            max_cnt = -1
            max_card = None
            for card, cnt in card_counts.items():
                if card == "J":
                    continue
                if cnt > max_cnt:
                    max_cnt, max_card = cnt, card
            card_counts[max_card] += card_counts["J"]
            del(card_counts["J"])
            #print(card_counts)
        
        card_types = len(card_counts.keys())
        if card_types == 1:
            return "FIVE"
        elif card_types == 2 and 4 in card_counts.values():
            return "FOUR"
        elif card_types == 2 and 3 in card_counts.values():
            return "FULL"
        elif card_types == 3 and 3 in card_counts.values():
            return "THREE"
        elif card_types == 3:
            return "TWO"
        elif card_types == 4:
            return "ONE"
        else:
            return "HIGH"
    @property
    def handtype_val(self):
        return HANDTYPES[self.handtype]
    def __eq__(self, other):
        return self.cards == other.cards
    def __lt__(self, other):
        if self.handtype_val < other.handtype_val:
            return True
        elif self.handtype_val > other.handtype_val:
            return False
        for sc, oc in zip(self.cards, other.cards):
            if sc == oc:
                continue
            return P2RANK[sc] < P2RANK[oc]
    def __repr__(self):
        return f"{self.cards} {self.handtype} {self.bid}"

def main(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f]
    hands = [Hand(line) for line in lines]
    hands.sort()
    p1_list = [(ind+1)*hand.bid for ind, hand in enumerate(hands)]
    print(f"Part 1: {sum(p1_list)}")

    hands2 = [P2Hand(line) for line in lines]
    hands2.sort()
    p2_list = [(ind+1)*hand.bid for ind, hand in enumerate(hands2)]
    print(f"Part 2: {sum(p2_list)}")

if __name__ == "__main__":
    main("samp01")
    main("input")
    