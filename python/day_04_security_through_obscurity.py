"""
Advent Of Code 2016
https://adventofcode.com/2016/day/4

--- Day 4: Security Through Obscurity ---
Finally, you come across an information kiosk with a list of rooms.  Of course, the list is encrypted and full of decoy data, but the instructions to decode the list are barely hidden nearby.  Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with ties broken by alphabetization.  For example:

aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
not-a-real-room-404[oarel] is a real room.
totally-real-room-200[decoy] is not.


Of the real rooms from the list above, the sum of their sector IDs is 1514.

What is the sum of the sector IDs of the real rooms?


--- Part Two ---
With all the decoy data out of the way, it's time to decrypt this list and get moving.

The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.

To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.

What is the sector ID of the room where North Pole objects are stored?

"""
from __future__ import annotations

from collections import Counter
from dataclasses import dataclass
from string import ascii_lowercase
from typing import Optional

import aoc2016


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


def solve_pt1(input_data: str) -> int:
    rooms = [Room.parse(line) for line in input_data.split("\n")]
    # solution_pt1: 185371
    return sum(room.sector_id for room in rooms if room.is_valid())


def solve_pt2(input_data: str, overwrite_sector_name: Optional[str] = None) -> int:
    rooms = [Room.parse(line) for line in input_data.split("\n")]
    if overwrite_sector_name is None:
        sector_name = "northpole object storage"
    else:
        sector_name = overwrite_sector_name
    room = [
        room for room in rooms if room.is_valid() and room.decrypt_name() == sector_name
    ][0]
    # solution_pt2: 984
    return room.sector_id


if __name__ == "__main__":
    tests = [
        aoc2016.AocTestCase(
            func=solve_pt1,
            output=1514,
            input_data="""aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]""",
        ),
        aoc2016.AocTestCase(
            func=solve_pt2,
            output=343,
            input_data="qzmt-zixmtkozy-ivhz-343[zimth]",
            func_args=("very encrypted name",),
        ),
    ]
    aoc2016.runner(solve_pt1, solve_pt2, tests)
