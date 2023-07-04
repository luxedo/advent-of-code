"""
Advent Of Code 2016


--- Day 1: No Time for a Taxicab ---
Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by stars. Unfortunately, the stars have been stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time to work them out further.

The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then, follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the destination?

For example:

Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
R5, L5, R5, R3 leaves you 12 blocks away.
How many blocks away is Easter Bunny HQ?


--- Part Two ---
Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?
"""
from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum

import aoc2016


class Orientation(Enum):
    NORTH = 0
    EAST = 1
    SOUTH = 2
    WEST = 3


class Side(Enum):
    LEFT = -1
    RIGHT = 1

    @classmethod
    def from_str(cls, s: str) -> Side:
        if s == "L":
            return cls.LEFT
        elif s == "R":
            return cls.RIGHT
        else:
            raise ValueError("Could not determine side")


@dataclass(frozen=True, eq=True)
class Coord:
    x: int = 0
    y: int = 0


@dataclass
class Taxi:
    orientation: Orientation = Orientation.NORTH
    position: Coord = field(default_factory=Coord)

    def turn(self, side: Side):
        self.orientation = Orientation(
            (self.orientation.value + side.value) % len(Orientation)
        )

    def step(self, steps: int):
        x, y = self.position.x, self.position.y
        match self.orientation:
            case Orientation.NORTH:
                y += steps
            case Orientation.EAST:
                x += steps
            case Orientation.SOUTH:
                y -= steps
            case Orientation.WEST:
                x -= steps
        self.position = Coord(x, y)

    def manhattan(self) -> int:
        return abs(self.position.x) + abs(self.position.y)


def parse_input(input_data: str) -> list[tuple[Side, int]]:
    return [(Side.from_str(d[0]), int(d[1:])) for d in input_data.strip().split(", ")]


def solve_pt1(input_data: str) -> int:
    taxi = Taxi()
    for side, steps in parse_input(input_data):
        taxi.turn(side)
        taxi.step(steps)
    return taxi.manhattan()


def solve_pt2(input_data: str) -> int:
    taxi = Taxi()
    seen = set([taxi.position])
    for side, steps in parse_input(input_data):
        taxi.turn(side)
        for _ in range(steps):
            taxi.step(1)
            if taxi.position in seen:
                return taxi.manhattan()
            seen.add(taxi.position)
    return taxi.manhattan()


if __name__ == "__main__":
    tests = [
        aoc2016.AocTestCase(func=solve_pt1, output=5, input_data="R2, L3"),
        aoc2016.AocTestCase(func=solve_pt1, output=2, input_data="R2, R2, R2"),
        aoc2016.AocTestCase(func=solve_pt1, output=12, input_data="R5, L5, R5, R3"),
        aoc2016.AocTestCase(func=solve_pt2, output=4, input_data="R8, R4, R4, R8"),
    ]
    aoc2016.runner(solve_pt1, solve_pt2, tests)
