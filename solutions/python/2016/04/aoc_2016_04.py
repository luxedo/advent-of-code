"""
ElfScript Brigade

Advent Of Code 2016 Day 04
Python Solution

Day 4: Security Through Obscurity

https://adventofcode.com/2016/day/4
"""
from __future__ import annotations

from collections import Counter
from dataclasses import dataclass
from string import ascii_lowercase


@dataclass
class Room:
    name: str
    sector_id: int
    checksum: str

    @classmethod
    def parse(cls, value: str) -> Room:
        head, tail = value.split("[")
        checksum = tail[:-1]
        *name_head, sector_id = head.split("-")
        name = " ".join(name_head)
        return Room(name, int(sector_id), checksum)

    def is_valid(self) -> bool:
        return self.checksum == self.compute_checksum()

    def compute_checksum(self) -> str:
        counter = Counter(self.name.replace(" ", ""))
        chars = sorted(dict(counter).items(), key=lambda x: (-x[1], x[0]))[:5]
        return "".join([c[0] for c in chars])

    def decrypt_name(self):
        return "".join(
            [
                chr(
                    ord("a")
                    + (ord(c) - ord("a") + self.sector_id) % len(ascii_lowercase)
                )
                if c != " "
                else " "
                for c in self.name
            ]
        )


def solve_pt1(input_data: str, _args: list[str] | None = None) -> int:
    rooms = [Room.parse(line) for line in input_data.split("\n")]
    # solution_pt1: 185371
    return sum(room.sector_id for room in rooms if room.is_valid())


def solve_pt2(input_data: str, args: list[str] | None = None) -> int:
    rooms = [Room.parse(line) for line in input_data.split("\n")]
    sector_name = "northpole object storage" if args is None else args[0]
    room = [
        room for room in rooms if room.is_valid() and room.decrypt_name() == sector_name
    ][0]
    # solution_pt2: 984
    return room.sector_id


if __name__ == "__main__":
    from esb.protocol import fireplacev1_0 as fp1_0

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fp1_0.run_solutions(solve_pt1, solve_pt2)
