#!/usr/bin/env python3

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

if __name__ == "__main__":
    with open("samp", "r") as f:
        line_orig = (line.strip() for line in f)
        lines = [line for line in line_orig if line != ""]

    lists_r = (build_list(line) for line in lines)
    lists = [liste for liste, _ in lists_r]

    for liste in lists:
        print(f"List: {liste}")
