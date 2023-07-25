"""
Advent Of Code {year} - day {day}
{url}

{description}
"""

from aoc.langs.python import runner


def solve_pt1(input_data: str) -> int:
    # solution_pt1: ???
    return 0


def solve_pt2(input_data: str) -> int:
    # solution_pt2: ???
    return 0


if __name__ == "__main__":
    tests = [
        runner.AocTestCase(func=solve_pt1, output=5, input_data="abc"),
        runner.AocTestCase(func=solve_pt2, output=4, input_data="def"),
    ]
    runner.run(solve_pt1, solve_pt2, tests)
