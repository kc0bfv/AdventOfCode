#!/usr/bin/env python3

from itertools import zip_longest
import functools

def grouper(iterable, n, *, incomplete='fill', fillvalue=None):
    "Collect data into non-overlapping fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, fillvalue='x') --> ABC DEF Gxx
    # grouper('ABCDEFG', 3, incomplete='strict') --> ABC DEF ValueError
    # grouper('ABCDEFG', 3, incomplete='ignore') --> ABC DEF
    args = [iter(iterable)] * n
    if incomplete == 'fill':
        return zip_longest(*args, fillvalue=fillvalue)
    if incomplete == 'strict':
        return zip(*args, strict=True)
    if incomplete == 'ignore':
        return zip(*args)
    else:
        raise ValueError('Expected fill, strict, or ignore')

def cmp_check_item(left, right):
  ret = check_item(left, right)
  if ret is None:
    raise RuntimeError("cmp was None")
  return -1 if ret else 1

def check_item(left, right):
  if isinstance(left, list) and isinstance(right, list):
    return check_list(left, right)
  elif isinstance(left, int) and isinstance(right, int):
    return check_int(left, right)
  elif isinstance(left, int):
    return check_list([left], right)
  elif isinstance(right, int):
    return check_list(left, [right])
  else:
    raise RuntimeError(f"Invalid state {left} {right}")

def check_int(left, right):
  #print("int", left, right)
  if left == right:
    return None
  return left < right

def check_list(left, right):
  # return true false or None for is correct, is not, or unknown
  #print(left, right)
  cur_entry = 0
  while True:
    #print(cur_entry)
    if len(left) <= cur_entry and len(right) <= cur_entry:
      return None
    elif len(left) == cur_entry and len(right) > cur_entry:
      return True
    elif len(left) > cur_entry and len(right) == cur_entry:
      return False
 
    check_ret = check_item(left[cur_entry], right[cur_entry])
    if check_ret != None:
      return check_ret
    cur_entry += 1
  return None

def build_list(chars, ind = 1):
    retval = list()
    curentry = None

    while ind < len(chars):
        char = chars[ind]
        if char == "]":
            if curentry is not None:
                retval.append(curentry)
            return retval, ind
        elif char == "[":
            assert(curentry is None)
            sublist, ind = build_list(chars, ind + 1)
            curentry = retval.append(sublist)
        elif char == ",":
            if curentry is not None:
                retval.append(curentry)
            curentry = None
        else:
            assert(ord("0") <= ord(char) <= ord("9"))
            if curentry is None:
                curentry = 0
            curentry *= 10
            curentry += int(char)
        ind += 1
    raise RuntimeError("Should never have gotten to end of build list")

def main(filename):
  with open(filename, "r") as f:
    built_lines = (build_list(line.strip()) for line in f if line.strip() != "")
    lines = [liste for liste, _ in built_lines]

  pair_up = [pair for pair in grouper(lines, 2)]

  right_order = [check_item(*pair) for pair in pair_up]
  #for pair in pair_up:
  #  print(pair)
  #print(right_order)
  p1 = sum([ind+1 for ind, val in enumerate(right_order) if val])
  print(f"Part 1: {p1}")

  lines += [[[2]], [[6]]]
  sortedlines = sorted(lines, key=functools.cmp_to_key(cmp_check_item))

  #print(sortedlines)
  p2 = (sortedlines.index([[2]])+1) * (sortedlines.index([[6]])+1)
  print(f"Part 2: {p2}")
  
if __name__ == "__main__":
  main("samp")
  main("input.txt")
