"""
ElfScript Brigade

Advent Of Code 2016 Day 14
Python Solution

Day 14: One-Time Pad

https://adventofcode.com/2016/day/14
"""

from dataclasses import dataclass
from hashlib import md5
from typing import Callable
from operator import itemgetter


@dataclass
class OneTimePad:
    salt: bytes
    hash_func: Callable
    index: int = 0

    @staticmethod
    def find_matches(h: str) -> tuple[dict[int, str], dict[int, str]]:
        triplets = {}
        fives = {}
        i = 0
        h = h + "\n"
        while i < len(h):
            c = h[i]
            for j in range(1, len(h) - i):
                if h[i] != h[i + j]:
                    if j >= 5:
                        fives[i - j] = c
                    if j >= 3:
                        triplets[i - j] = c
                    i += j - 1
                    break
            i += 1
        return triplets, fives

    def run(self, iters: int):
        candidates: dict[int, tuple[str, str]] = {}
        keys = []
        while len(keys) < iters:
            h = self.hash_func(self.salt + str(self.index).encode("ascii"))
            triplets, fives = self.find_matches(h)

            for v in sorted(triplets.values()):
                candidates[self.index] = (v, h)
                break

            candidates = {k: v for k, v in candidates.items() if k + 1000 >= self.index}

            new_keys = []
            del_keys = []
            for k, (cv, ch) in candidates.items():
                new_keys.extend(
                    [
                        (fv, k, self.index, ch, h)
                        for fv in fives.values()
                        if fv == cv and k != self.index
                    ]
                )
                del_keys.extend([k for _, k, _, _, _ in new_keys])
            for k in del_keys:
                if k in candidates:
                    del candidates[k]
            keys.extend(new_keys)
            # if self.index == 22551:
            #     print(1, h)
            #     print(triplets)
            #     print()
            # if self.index == 22859:
            #     print(2, h)
            #     print(fives)
            #     print()

            self.index += 1
        return keys


def solve_pt1(input_data: str, args: list[str] | None = None) -> int:
    otp = OneTimePad(input_data.strip().encode("ascii"), lambda x: md5(x).hexdigest())
    keys = otp.run(64)
    for i, k in enumerate(sorted(keys, key=itemgetter(1))):
        print(i, k)
    return sorted(keys, key=itemgetter(1))[64][1]


def solve_pt2(input_data: str, args: list[str] | None = None) -> int:
    def _stretch_hash(value):
        return md5(value).hexdigest().encode("ascii")

    def stretch_hash(value):
        for i in range(2017):
            value = _stretch_hash(value)
        return value.decode("ascii")
    otp = OneTimePad(input_data.strip().encode("ascii"), stretch_hash)
    keys = otp.run(64)
    return keys[63][1]


if __name__ == "__main__":
    from esb.protocol import fireplace

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
