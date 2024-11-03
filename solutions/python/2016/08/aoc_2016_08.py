"""
ElfScript Brigade

Advent Of Code 2016 Day 08
Python Solution

Day 8: Two-Factor Authentication

https://adventofcode.com/2016/day/8
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, Type


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


def solve(input_data: str) -> Screen:
    screen = Screen()
    for cmd_str in input_data.splitlines():
        command: RectCommand | RotateCommand
        try:
            command = RectCommand.parse(cmd_str)
        except AssertionError:
            command = RotateCommand.parse(cmd_str)
        screen.run(command)
    return screen


def solve_pt1(input_data: str, _args: list[str] | None = None) -> int:
    screen = solve(input_data)
    # solution_pt1: 128
    return screen.lit


def solve_pt2(input_data: str, _args: list[str] | None = None) -> str:
    screen = solve(input_data)
    # solution_pt2: EOARGPHYAO
    # print()
    # for x in str(screen):
    #     if x == "\n":
    #         print()
    #         continue
    #     print(f"{ord(x)} ", end="")
    return "\n" + str(screen)


if __name__ == "__main__":
    from esb.protocol import fireplace

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
