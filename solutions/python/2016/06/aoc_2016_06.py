"""
ElfScript Brigade

Advent Of Code 2016 Day 06
Python Solution

Day 6: Signals and Noise

https://adventofcode.com/2016/day/6
"""
from collections import Counter


def solve_pt1(input_data: str, _args: list[str] | None = None) -> str:
    word_len = len(input_data.strip().partition("\n")[0])
    counters: list[Counter] = [Counter() for _ in range(word_len)]
    for line in input_data.split():
        for i in range(word_len):
            counters[i].update([line[i]])
    # solution_pt1: umejzgdw
    return "".join([c.most_common()[0][0] for c in counters])


def solve_pt2(input_data: str, _args: list[str] | None = None) -> str:
    word_len = len(input_data.strip().partition("\n")[0])
    counters: list[Counter] = [Counter() for _ in range(word_len)]
    for line in input_data.split():
        for i in range(word_len):
            counters[i].update([line[i]])
    # solution_pt2: aovueakv
    return "".join([c.most_common()[-1][0] for c in counters])


if __name__ == "__main__":
    from esb.protocol import fireplacev1_0 as fp1_0

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fp1_0.run_solutions(solve_pt1, solve_pt2)
