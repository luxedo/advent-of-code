"""
  Advent Of Code 2018 - day 03
  https://adventofcode.com/2018/day/3

    --- Day 3: No Matter How You Slice It ---
  The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to
  someone who helpfully wrote its box IDs on the wall of the warehouse in the middle of the night).
  Unfortunately, anomalies are still affecting them - nobody can even agree on how to cut the
  fabric.

  The whole piece of fabric they're working on is a very large square - at least 1000 inches on each
  side.

  Each Elf has made a claim about which area of fabric would be ideal for Santa's suit.  All claims
  have an ID and consist of a single rectangle with edges parallel to the edges of the fabric.  Each
  claim's rectangle is defined as follows:

  The number of inches between the left edge of the fabric and the left edge of the rectangle.
  The number of inches between the top edge of the fabric and the top edge of the rectangle.
  The width of the rectangle in inches.
  The height of the rectangle in inches.


  A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left
  edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square
  inches of fabric represented by # (and ignores the square inches of fabric represented by .) in
  the diagram below:
  ...........
  ...........
  ...#####...
  ...#####...
  ...#####...
  ...#####...
  ...........
  ...........
  ...........


  The problem is that many of the claims overlap, causing two or more claims to cover part of the
  same areas.  For example, consider the following claims:
  #1 @ 1,3: 4x4
  #2 @ 3,1: 4x4
  #3 @ 5,5: 2x2


  Visually, these claim the following areas:
  ........
  ...2222.
  ...2222.
  .11XX22.
  .11XX22.
  .111133.
  .111133.
  ........


  The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the
  others, does not overlap either of them.)

  If the Elves all proceed with their own plans, none of them will have enough fabric. How many
  square inches of fabric are within two or more claims?

  --- Part Two ---
  Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch
  of fabric with any other claim. If you can somehow draw attention to it, maybe the Elves will be
  able to make Santa's suit after all!

  For example, in the claims above, only claim 3 is intact after all claims are made.

  What is the ID of the only claim that doesn't overlap?
"""

import re
from dataclasses import dataclass, field

from aoc.langs.python import runner


@dataclass
class Claim:
    index: int
    x: int
    y: int
    w: int
    h: int


@dataclass
class Fabric:
    sides: int
    fabric: list[list[list[int]]] = field(init=False)

    def __post_init__(self):
        self.fabric = [[[] for _ in range(self.sides)] for _ in range(self.sides)]

    def claim(self, claim: Claim):
        for y in range(claim.y, claim.y + claim.h):
            for x in range(claim.x, claim.x + claim.w):
                self.fabric[y][x].append(claim.index)

    def claim_all(self, claims: list[Claim]):
        for claim in claims:
            self.claim(claim)

    def count_overlap(self) -> int:
        return sum([len(c) > 1 for line in self.fabric for c in line])

    def not_overlapping(self) -> set[int]:
        indexes = set([i for line in self.fabric for c in line for i in c])
        overlapping = set(
            [i for line in self.fabric for c in line for i in c if len(c) > 1]
        )
        return indexes - overlapping

    @staticmethod
    def parse_claims(input_data: str) -> list[Claim]:
        claims = [
            re.match(r"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$", line)
            for line in input_data.split("\n")
        ]
        return [
            Claim(index=int(m[1]), x=int(m[2]), y=int(m[3]), w=int(m[4]), h=int(m[5]))
            for m in claims
        ]

    def __str__(self):
        return "\n".join(
            ["".join(["#" if len(c) else "." for c in line]) for line in self.fabric]
        )


def solve_pt1(input_data: str) -> int:
    claims = Fabric.parse_claims(input_data)
    max_side = max([max([c.y + c.h, c.x + c.w]) for c in claims])
    fabric = Fabric(max_side + 1)
    fabric.claim_all(claims)
    # solution_pt1: 112418
    return fabric.count_overlap()


def solve_pt2(input_data: str) -> int:
    claims = Fabric.parse_claims(input_data)
    max_side = max([max([c.y + c.h, c.x + c.w]) for c in claims])
    fabric = Fabric(max_side + 1)
    fabric.claim_all(claims)
    # solution_pt2: 560
    return fabric.not_overlapping().pop()


if __name__ == "__main__":
    tests = [
        runner.AocTestCase(
            func=solve_pt1,
            output=4,
            input_data="#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2",
        ),
        runner.AocTestCase(
            func=solve_pt2,
            output=3,
            input_data="#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2",
        ),
    ]
    runner.run(solve_pt1, solve_pt2, tests)
