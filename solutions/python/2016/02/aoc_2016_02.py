"""
ElfScript Brigade

Advent Of Code 2016 Day 02
Python Solution

Day 2: Bathroom Security

https://adventofcode.com/2016/day/2
"""
from __future__ import annotations

from collections.abc import Iterable
from enum import Enum


class Direction(Enum):
    UP = (0, -1)
    RIGHT = (1, 0)
    DOWN = (0, 1)
    LEFT = (-1, 0)

    @classmethod
    def from_str(cls, s: str) -> Direction:
        match s:
            case "U":
                return cls.UP
            case "R":
                return cls.RIGHT
            case "D":
                return cls.DOWN
            case "L":
                return cls.LEFT
            case _:
                print(s)
                raise ValueError("Could not parse direction")


class Keypad:
    def __init__(self, pad: list[list[str]], starting_position: str):
        self.pad = pad
        self.starting_position = starting_position
        self.max_x = len(pad[0])
        self.max_y = len(pad[1])
        self.reset()

    def reset(self):
        for y, line in enumerate(self.pad):
            for x, cell in enumerate(line):
                if cell == self.starting_position:
                    self.finger = [x, y]
                    return
        else:
            raise ValueError("Could not find starting position")

    def move(self, direction: Direction) -> str:
        self.finger[0] += direction.value[0]
        self.finger[1] += direction.value[1]
        self.clamp()
        digit = self.pad[self.finger[1]][self.finger[0]]
        if digit == " ":
            self.finger[0] -= direction.value[0]
            self.finger[1] -= direction.value[1]
            digit = self.pad[self.finger[1]][self.finger[0]]
        return digit

    def move_sequence(self, sequence: Iterable[Direction]) -> str:
        for direction in sequence:
            digit = self.move(direction)
        return digit

    def clamp(self):
        if self.finger[0] < 0:
            self.finger[0] = 0
        elif self.finger[0] >= self.max_x:
            self.finger[0] = self.max_x - 1
        if self.finger[1] < 0:
            self.finger[1] = 0
        elif self.finger[1] >= self.max_y:
            self.finger[1] = self.max_y - 1


def solve_pt1(input_data: str, _args: list[str] | None = None) -> str:
    pad = [["1", "2", "3"], ["4", "5", "6"], ["7", "8", "9"]]
    starting_position = "5"
    keypad = Keypad(pad, starting_position)
    code = ""
    for line in input_data.split():
        digit = keypad.move_sequence([Direction.from_str(c) for c in line])
        code += digit
    # solution_pt1: 84452
    return code


def solve_pt2(input_data: str, _args: list[str] | None = None) -> str:
    pad = [
        [" ", " ", "1", " ", " "],
        [" ", "2", "3", "4", " "],
        ["5", "6", "7", "8", "9"],
        [" ", "A", "B", "C", " "],
        [" ", " ", "D", " ", " "],
    ]
    starting_position = "5"
    keypad = Keypad(pad, starting_position)
    code = ""
    for line in input_data.split():
        digit = keypad.move_sequence([Direction.from_str(c) for c in line])
        code += digit
    # solution_pt2: D65C3
    return code


if __name__ == "__main__":
    from esb.protocol import fireplacev1_0 as fp1_0

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fp1_0.run_solutions(solve_pt1, solve_pt2)
