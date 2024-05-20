"""
ElfScript Brigade

Advent Of Code 2016 Day 09
Python Solution

Day 9: Explosives in Cyberspace

https://adventofcode.com/2016/day/9
"""
from __future__ import annotations

from dataclasses import dataclass
from string import ascii_uppercase
from typing import List


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


def solve_pt1(input_data: str, _args: list[str] | None = None) -> int:
    t = TokenizerV1(input_data)
    # solution_pt1: 138735
    return len(t)


def solve_pt2(input_data: str, _args: list[str] | None = None) -> int:
    t = TokenizerV2(input_data)
    # solution_pt2: 11125026826
    return len(t)


if __name__ == "__main__":
    from esb.protocol import fireplace
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
