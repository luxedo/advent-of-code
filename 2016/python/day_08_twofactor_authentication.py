"""
Advent Of Code 2016
https://adventofcode.com/2016/day/8

--- Day 8: Two-Factor Authentication ---
You come across a door implementing what you can only assume is an implementation of two-factor authentication after a long game of requirements telephone.

To get past the door, you first swipe a keycard (no problem; there was one on a nearby desk). Then, it displays a code on a little screen, and you type that code on a keypad. Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed. After a few minutes, you've taken everything apart and figured out how it works. Now you just have to work out what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your puzzle input. The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat peculiar operations:

rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall off the right end appear at the left end of the row.
rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that would fall off the bottom appear at the top of the column.


For example, here is a simple sequence on a smaller screen:


rect 3x2 creates a small rectangle in the top-left corner:###....
###....
.......

rotate column x=1 by 1 rotates the second column down by one pixel:#.#....
###....
.#.....

rotate row y=0 by 4 rotates the top row right by four pixels:....#.#
###....
.#.....

rotate column x=1 by 1 again rotates the second column down by one pixel, causing the bottom pixel to wrap back to the top:.#..#.#
#.#....
.#.....


As you can see, this display technology is extremely powerful, and will soon dominate the tiny-code-displaying-screen market.  That's what the advertisement on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen did work, how many pixels should be lit?


--- Part Two ---
You notice that the screen is only capable of displaying capital letters; in the font it uses, each letter is 5 pixels wide and 6 tall.

After you swipe your card, what code is the screen trying to display?
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, Type

from aoc.langs.python import runner


@dataclass
class RectCommand:
    row: int
    col: int

    @classmethod
    def parse(cls: Type[RectCommand], cmd_str: str) -> RectCommand:
        command, *args = cmd_str.split()
        assert command == "rect", f"Cannot parse command {cmd_str}"
        col, row = map(int, args[0].split("x"))
        return cls(
            row=row,
            col=col,
        )


@dataclass
class RotateCommand:
    row: Optional[int]
    col: Optional[int]
    by: int

    @classmethod
    def parse(cls: Type[RotateCommand], cmd_str: str) -> RotateCommand:
        command, *args = cmd_str.split()
        assert command == "rotate", f"Cannot parse command {cmd_str}"
        axis, _index, _, _by = args
        index = int(_index.split("=")[1])
        by = int(_by)
        row, col = None, None
        match axis:
            case "column":
                col = index
            case "row":
                row = index
            case "_":
                raise ValueError(f"Cannot parse command {cmd_str}")
        return cls(
            row=row,
            col=col,
            by=by,
        )


@dataclass
class Screen:
    cols: int = 50
    rows: int = 6

    def __post_init__(self):
        self.screen = [[" " for _ in range(self.cols)] for _ in range(self.rows)]

    def run(self, command: RectCommand | RotateCommand):
        match command:
            case RectCommand(row, col):
                for r in range(row):
                    for c in range(col):
                        self.screen[r][c] = "#"
            case RotateCommand(row, col, by):
                if row is not None:
                    for _ in range(by):
                        c = self.screen[row].pop()
                        self.screen[row] = [c] + self.screen[row]
                elif col is not None:
                    for _ in range(by):
                        c = self.screen[-1][col]
                        for r in range(self.rows - 1, 0, -1):
                            self.screen[r][col] = self.screen[r - 1][col]
                        self.screen[0][col] = c

    @property
    def lit(self) -> int:
        return sum([row.count("#") for row in self.screen])

    def __str__(self) -> str:
        return "\n".join("".join(col) for col in self.screen)


def solve_pt1(input_data: str) -> int:
    screen = Screen()
    for cmd_str in input_data.splitlines():
        command: RectCommand | RotateCommand
        try:
            command = RectCommand.parse(cmd_str)
        except AssertionError:
            command = RotateCommand.parse(cmd_str)
        screen.run(command)
    # solution_pt1: 128
    return screen.lit


def solve_pt2(input_data: str) -> str:
    screen = Screen()
    for cmd_str in input_data.splitlines():
        command: RectCommand | RotateCommand
        try:
            command = RectCommand.parse(cmd_str)
        except AssertionError:
            command = RotateCommand.parse(cmd_str)
        screen.run(command)
    # solution_pt2: EOARGPHYAO
    return "\n" + str(screen)


if __name__ == "__main__":
    input_data = """rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1"""
    ans_2 = """
    # #
# #
 #
 #

                                                  """
    tests = [
        runner.AocTestCase(func=solve_pt1, output=6, input_data=input_data),
        runner.AocTestCase(func=solve_pt2, output=ans_2, input_data=input_data),
    ]
    runner.run(solve_pt1, solve_pt2, tests)
