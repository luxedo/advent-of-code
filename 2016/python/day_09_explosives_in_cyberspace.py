"""
Advent Of Code 2016
https://adventofcode.com/2016/day/9

--- Day 9: Explosives in Cyberspace ---
Wandering around a secure area, you come across a datalink port to a new part of the network. After briefly scanning it for interesting files, you find one file in particular that catches your attention. It's compressed with an experimental format, but fortunately, the documentation for the format is nearby.

The format compresses a sequence of characters. Whitespace is ignored. To indicate that some sequence should be repeated, a marker is added to the file, like (10x2). To decompress this marker, take the subsequent 10 characters and repeat them 2 times. Then, continue reading the file after the repeated data.  The marker itself is not included in the decompressed output.

If parentheses or other characters appear within the data referenced by a marker, that's okay - treat it like normal data, not a marker, and then resume looking for markers after the decompressed section.

For example:

ADVENT contains no markers and decompresses to itself with no changes, resulting in a decompressed length of 6.
A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a decompressed length of 7.
(3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a decompressed length of 11.
(6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but because it's within a data section of another marker, it is not treated any differently from the A that comes after it. It has a decompressed length of 6.
X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of 18), because the decompressed data from the (8x2) marker (the (3x3)ABC) is skipped and not processed further.


What is the decompressed length of the file (your puzzle input)? Don't count whitespace.


--- Part Two ---
Apparently, the file actually uses version two of the format.

In version two, the only difference is that markers within decompressed data are decompressed. This, the documentation explains, provides much more substantial compression capabilities, allowing many-gigabyte files to be stored in only a few kilobytes.

For example:

(3x3)XYZ still becomes XYZXYZXYZ, as the decompressed section contains no markers.
X(8x2)(3x3)ABCY becomes XABCABCABCABCABCABCY, because the decompressed data from the (8x2) marker is then further decompressed, thus triggering the (3x3) marker twice for a total of six ABC sequences.
(27x12)(20x12)(13x14)(7x10)(1x12)A decompresses into a string of A repeated 241920 times.
(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN becomes 445 characters long.
Unfortunately, the computer you brought probably doesn't have enough memory to actually decompress the file; you'll have to come up with another way to get its decompressed length.

What is the decompressed length of the file using this improved format?
"""
from __future__ import annotations

from dataclasses import dataclass
from string import ascii_uppercase
from typing import List

from aoc.langs.python import runner


@dataclass
class Token:
    token: str | List[Token]
    mul: int

    def __len__(self):
        if isinstance(self.token, str):
            return len(self.token) * self.mul
        else:
            return sum(len(token) for token in self.token) * self.mul

    def __str__(self):
        if isinstance(self.token, str):
            return self.token * self.mul
        else:
            return "".join(str(token) for token in self.token) * self.mul


@dataclass
class TokenizerV1:
    source: str

    def __post_init__(self):
        self.source = self.source.replace(" ", "")

    def __len__(self):
        return sum(len(t) for t in self.parsed)

    @property
    def parsed(self) -> List[Token]:
        if len(self.source) == 0:
            return []
        self._parsed: List[Token]
        if hasattr(self, "_parsed"):
            return self._parsed

        char = self.source[0]
        if char in ascii_uppercase:
            self._parsed = [Token(char, 1)] + self.__class__(self.source[1:]).parsed
            return self.parsed
        if char == "(":
            block, idx = self._parse_block(self.source)
            self._parsed = block + self.__class__(self.source[idx:]).parsed
            return self.parsed
        raise ValueError("unreachable")

    def _parse_block(self, source):
        rule, tail = source.removeprefix("(").split(")", 1)
        step, mul = map(int, rule.split("x"))
        chars = tail[:step]
        return [Token(chars, mul)], len(rule) + 2 + step

    @property
    def compiled(self) -> str:
        if not hasattr(self, "_compiled"):
            self._compiled = "".join(str(t) for t in self.parsed)
        return self._compiled


class TokenizerV2(TokenizerV1):
    def _parse_block(self, source):
        rule, tail = source.removeprefix("(").split(")", 1)
        step, mul = map(int, rule.split("x"))
        chars = tail[:step]
        idx = len(rule) + 2 + step
        if "x" in chars:
            return [Token(self.__class__(chars).parsed, mul)], idx
        return [Token(chars, mul)], idx


def solve_pt1(input_data: str) -> int:
    t = TokenizerV1(input_data)
    # solution_pt1: 138735
    return len(t)


def solve_pt2(input_data: str) -> int:
    t = TokenizerV2(input_data)
    # solution_pt2: 11125026826
    return len(t)


if __name__ == "__main__":
    tests = [
        runner.AocTestCase(func=solve_pt1, output=6, input_data="ADVENT"),
        runner.AocTestCase(func=solve_pt1, output=7, input_data="A(1x5)BC"),
        runner.AocTestCase(func=solve_pt1, output=9, input_data="(3x3)XYZ"),
        runner.AocTestCase(func=solve_pt1, output=11, input_data="A(2x2)BCD(2x2)EFG"),
        runner.AocTestCase(func=solve_pt1, output=6, input_data="(6x1)(1x3)A"),
        runner.AocTestCase(func=solve_pt1, output=18, input_data="X(8x2)(3x3)ABCY"),
        runner.AocTestCase(func=solve_pt2, output=9, input_data="(3x3)XYZ"),
        runner.AocTestCase(func=solve_pt2, output=20, input_data="X(8x2)(3x3)ABCY"),
        runner.AocTestCase(
            func=solve_pt2,
            output=241920,
            input_data="(27x12)(20x12)(13x14)(7x10)(1x12)A",
        ),
        runner.AocTestCase(
            func=solve_pt2,
            output=445,
            input_data="(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN",
        ),
    ]
    runner.run(solve_pt1, solve_pt2, tests)
