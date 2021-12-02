#!/usr/bin/env python3

import re

def read(filename):
    with open(filename, "r") as fin:
        lines = [_.strip() for _ in fin]

    records = [""]
    for line in lines:
        if line == "":
            records.append("")
        else:
            records[-1] += " " + line

    records_nice = list()
    for record in records:
        words_1 = record.strip().split(" ")
        words = [word.strip() for word in words_1 if word.strip() != ""]

        content = [word.split(":", 1) for word in words]
        records_nice.append(content)

    return records_nice

def check(rec_n):
    nice_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
    valid = 0
    valid_2 = 0
    for record in rec_n:
        found_flds = set()
        for fld_n, fld_c in record:
            if fld_n in nice_fields:
                found_flds.add(fld_n)
        if (len(found_flds) == 7 and "cid" not in found_flds) or \
                len(found_flds) == 8:
            valid += 1
            if further_valid(record):
                valid_2 += 1
        print(record)
        print(found_flds)
        print(valid, valid_2)

    return valid, valid_2

def further_valid(record):
    rec_d = {fld_n: fld_c for (fld_n, fld_c) in record}

    if rec_d["hgt"].endswith("cm"):
        if not (150 <= int(rec_d["hgt"][:-2]) <= 193):
            print("a")
            return False
    elif rec_d["hgt"].endswith("in"):
        if not (59 <= int(rec_d["hgt"][:-2]) <= 76):
            print("b")
            return False
    else:
        print("b2")
        return False

    if rec_d["ecl"] not in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]:
        print("c")
        return False

    if not (1920 <= int(rec_d["byr"]) <= 2002 and \
        2010 <= int(rec_d["iyr"]) <= 2020 and \
        2020 <= int(rec_d["eyr"]) <= 2030) :
        print("d1")
        return False

    if not (rec_d["hcl"][0] == "#" and re.match("[0-9a-f]{6}", rec_d["hcl"][1:])):
        print("d2")
        return False

    if not (re.match("[0-9]{9}$", rec_d["pid"])):
        print("d3")
        return False

    print("e")
    return True
        


if __name__ == "__main__":
    records = read("input.txt")
    print(check(records))
