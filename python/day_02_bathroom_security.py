"""
Advent Of Code 2016

--- Day 2: Bathroom Security ---
You arrive at Easter Bunny Headquarters under cover of darkness. However, you left in such a rush that you forgot to use the bathroom! Fancy office buildings like this one usually have keypad locks on their bathrooms, so you search the front desk for the code.

"In order to improve security," the document you find says, "bathroom codes will no longer be written down. Instead, please memorize and follow the procedure below to access the bathrooms."

The document goes on to explain that each button to be pressed can be found by starting on the previous button and moving to adjacent buttons on the keypad: U moves up, D moves down, L moves left, and R moves right. Each line of instructions corresponds to one button, starting at the previous button (or, for the first line, the "5" button); press whatever button you're on at the end of each line. If a move doesn't lead to a button, ignore it.

You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. You picture a keypad like this:

1 2 3
4 5 6
7 8 9
Suppose your instructions are:

ULL
RRDDD
LURDL
UUUUD
You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and stay on "1"), so the first button is 1.
Starting from the previous button ("1"), you move right twice (to "3") and then down three times (stopping at "9" after two moves and ignoring the third), ending up with 9.
Continuing from "9", you move left, up, right, down, and left, ending with 8.
Finally, you move up four times (stopping at "2"), then down once, ending with 5.
So, in this example, the bathroom code is 1985.

Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?


--- Part Two ---
You finally arrive at the bathroom (it's a several minute walk from the lobby so visitors can behold the many fancy conference rooms and water coolers on this floor) and go to punch in the code. Much to your bladder's dismay, the keypad is not at all like you imagined it. Instead, you are confronted with the result of hundreds of man-hours of bathroom-keypad-design meetings:

    1
  2 3 4
5 6 7 8 9
  A B C
    D
You still start at "5" and stop when you're at an edge, but given the same instructions as above, the outcome is very different:

You start at "5" and don't move at all (up and left are both edges), ending at 5.
Continuing from "5", you move right twice and down three times (through "6", "7", "B", "D", "D"), ending at D.
Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
Finally, after five more moves, you end at 3.
So, given the actual keypad layout, the code would be 5DB3.

Using the same instructions in your puzzle input, what is the correct bathroom code?
"""
from __future__ import annotations

from collections.abc import Iterable
from enum import Enum

import aoc2016


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


def solve_pt1(input_data: str) -> str:
    pad = [["1", "2", "3"], ["4", "5", "6"], ["7", "8", "9"]]
    starting_position = "5"
    keypad = Keypad(pad, starting_position)
    code = ""
    for line in input_data.split():
        digit = keypad.move_sequence([Direction.from_str(c) for c in line])
        code += digit
    # solution_pt1: 84452
    return code


def solve_pt2(input_data: str) -> str:
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
    test_input = """ULL
RRDDD
LURDL
UUUUD"""
    tests = [
        aoc2016.AocTestCase(
            func=solve_pt1,
            output="1985",
            input_data=test_input,
        ),
        aoc2016.AocTestCase(func=solve_pt2, output="5DB3", input_data=test_input),
    ]
    aoc2016.runner(solve_pt1, solve_pt2, tests)
