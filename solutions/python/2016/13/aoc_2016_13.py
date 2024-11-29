"""
ElfScript Brigade

Advent Of Code 2016 Day 13
Python Solution

Day 13: A Maze of Twisty Little Cubicles

https://adventofcode.com/2016/day/13
"""

from dataclasses import dataclass, field
from functools import lru_cache


@dataclass(frozen=True)
class Board:
    h: int
    w: int
    favorite: int
    py: int
    px: int
    board: list[list[bool]] = field(compare=False, hash=False)

    @classmethod
    def new(cls, h: int, w: int, favorite: int, py: int, px: int):
        board = cls.expand(h, w, favorite)
        return cls(h, w, favorite, py, px, board)

    def __repr__(self) -> str:
        return f"py: {self.py}, px: {self.px}\n" + "\n".join(
            "".join(
                "#" if not c else ("." if x != self.px or y != self.py else "O")
                for x, c in enumerate(row)
            )
            for y, row in enumerate(self.board)
        )

    @staticmethod
    @lru_cache
    def is_wall(i: int, j: int, favorite: int) -> bool:
        def func(y: int, x: int) -> int:
            return x**2 + 3 * x + 2 * x * y + y + y**2 + favorite

        return (bin(func(i, j))[2:].count("1") % 2) == 0

    @classmethod
    def expand(cls, h: int, w: int, favorite: int) -> list[list[bool]]:
        y, x = 0, 0
        new_board = []
        for i in range(y, y + h):
            new_board.append([cls.is_wall(i, j, favorite) for j in range(x, x + w)])
        return new_board

    def get(self, y: int, x: int) -> bool:
        return self.board[y][x]

    def move(self):
        avaliable_moves = [
            (ny, nx)
            for ny, nx in [
                (self.py - 1, self.px),
                (self.py, self.px - 1),
                (self.py + 1, self.px),
                (self.py, self.px + 1),
            ]
            if ny >= 0 and nx >= 0 and ny < self.h and nx < self.w and self.get(ny, nx)
        ]

        return [
            self.new(self.h, self.w, self.favorite, ny, nx)
            for ny, nx in avaliable_moves
        ]


def solve_pt1(input_data: str, args: list[str] | None = None) -> int:
    fx, fy = [int(a) for a in args] if args is not None else [31, 39]
    border = 5
    boards = [Board.new(fy + border, fx + border, int(input_data), 1, 1)]
    final = Board.new(fy + border, fx + border, int(input_data), py=fy, px=fx)
    seen = set(boards)
    c = 0
    while final not in seen and len(boards) > 0:
        boards = [m for b in boards for m in b.move() if m not in seen]
        c += 1
        seen |= set(boards)
    return c


def solve_pt2(input_data: str, args: list[str] | None = None) -> int:
    fx, fy = [int(a) for a in args] if args is not None else [31, 39]
    border = 5
    boards = [Board.new(fy + border, fx + border, int(input_data), 1, 1)]
    seen = set(boards)
    for _ in range(50):
        boards = [m for b in boards for m in b.move() if m not in seen]
        seen |= set(boards)
    return len(seen)


if __name__ == "__main__":
    from esb.protocol import fireplace

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
