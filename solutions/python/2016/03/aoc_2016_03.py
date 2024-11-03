"""
ElfScript Brigade

Advent Of Code 2016 Day 03
Python Solution

Day 3: Squares With Three Sides

https://adventofcode.com/2016/day/3
"""

from dataclasses import dataclass


@dataclass(order=True)
class Triangle:
    side_a: int
    side_b: int
    side_c: int

    def __init__(self, *sides):
        if len(sides) != 3:
            raise ValueError("A triangle has three sides you dummy!")
        self.side_a, self.side_b, self.side_c = sorted(sides)
        if self.side_a + self.side_b <= self.side_c:
            raise ArithmeticError("Impossible triangle")


def solve_pt1(input_data: str, _args: list[str] | None = None) -> int:
    triangles = []
    triangle_sides = [
        [int(v) for v in line.split()] for line in input_data.strip().split("\n")
    ]
    for sides in triangle_sides:
        try:
            triangles.append(Triangle(*sides))
        except ArithmeticError:
            pass
    # solution_pt1: 993
    return len(triangles)


def solve_pt2(input_data: str, _args: list[str] | None = None) -> int:
    triangles = []
    triangle_sides = [
        [int(v) for v in line.split()] for line in input_data.strip().split("\n")
    ]
    triangle_sides_t = list(zip(*triangle_sides))
    for line in triangle_sides_t:
        for i in range(len(line) // 3):
            try:
                triangles.append(Triangle(*line[3 * i : 3 * (i + 1)]))
            except ArithmeticError:
                pass
    # solution_pt2: 1849
    return len(triangles)


if __name__ == "__main__":
    from esb.protocol import fireplace

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
