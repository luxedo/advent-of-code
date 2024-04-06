"""
ElfScript Brigade

Advent Of Code 2016 Day 05
Python Solution

Day 5: How About a Nice Game of Chess?

https://adventofcode.com/2016/day/5
"""
from hashlib import md5


def find_ecp(cipher: str) -> str:
    i = 0
    key = ""
    while len(key) < 8:
        attempt = cipher + str(i)
        m = md5(attempt.encode("ascii")).hexdigest()
        if m.startswith("00000"):
            key += m[5]
        i += 1
    return key


def find_aecp(cipher: str) -> str:
    i = 0
    key = [""] * 8
    while any([k == "" for k in key]):
        attempt = cipher + str(i)
        m = md5(attempt.encode("ascii")).hexdigest()
        if m.startswith("00000"):
            idx = int(m[5], base=16)
            if idx < len(key) and key[idx] == "":
                key[idx] = m[6]
        i += 1
    return "".join(key)


def solve_pt1(input_data: str, args: list[str] | None = None) -> str:
    # solution_pt1: f97c354d
    return find_ecp(input_data)


def solve_pt2(input_data: str, args: list[str] | None = None) -> str:
    # solution_pt2: 863dde27
    return find_aecp(input_data)


if __name__ == "__main__":
    from esb.protocol import fireplacev1_0 as fp1_0

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fp1_0.run_solutions(solve_pt1, solve_pt2)
