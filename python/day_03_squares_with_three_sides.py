"""
Advent Of Code 2016
https://adventofcode.com/2016/day/3

--- Day 3: Squares With Three Sides ---
Now that you can think clearly, you move deeper into the labyrinth of hallways and office furniture that makes up this part of Easter Bunny HQ. This must be a graphic design department; the walls are covered in specifications for triangles.

Or are they?

The design document gives the side lengths of each triangle it describes, but... 5 10 25?  Some of these aren't triangles. You can't help but mark the impossible ones.

In a valid triangle, the sum of any two sides must be larger than the remaining side.  For example, the "triangle" given above is impossible, because 5 + 10 is not larger than 25.

In your puzzle input, how many of the listed triangles are possible?


--- Part Two ---
Now that you've helpfully marked up their design documents, it occurs to you that triangles are specified in groups of three vertically. Each set of three numbers in a column specifies a triangle. Rows are unrelated.

For example, given the following specification, numbers with the same hundreds digit would be part of the same triangle:

101 301 501
102 302 502
103 303 503
201 401 601
202 402 602
203 403 603
In your puzzle input, and instead reading by columns, how many of the listed triangles are possible?
"""
from dataclasses import dataclass

import aoc2016


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


def solve_pt1(input_data: str) -> int:
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


def solve_pt2(input_data: str) -> int:
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
    tests = [
        aoc2016.AocTestCase(func=solve_pt1, output=0, input_data="5 10 25"),
        aoc2016.AocTestCase(
            func=solve_pt2,
            output=6,
            input_data="""101 301 501
102 302 502
103 303 503
201 401 601
202 402 602
203 403 603
""",
        ),
    ]
    aoc2016.runner(solve_pt1, solve_pt2, tests)
