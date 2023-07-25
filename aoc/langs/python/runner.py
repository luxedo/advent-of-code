"""
Advent of Code Runner

Fetches and runs code for the solutions.

Examples:
    python -m aoc --year 2022 --day 3 fetch python
    python -m aoc --year 2021 --day 13 run rust
"""
import argparse
import unittest
from collections.abc import Callable
from pathlib import Path
from typing import Union

import __main__


class AocTestCase(unittest.TestCase):
    def __init__(
        self,
        func: Callable,
        output: Union[int, str],
        input_data: str,
        func_args: tuple = (),
    ):
        self.func = func
        self.output = output
        self.input_data = input_data
        self.func_args = func_args
        super().__init__()

    def runTest(self):
        self.assertEqual(self.output, self.func(self.input_data, *self.func_args))


def load_input() -> str:
    main_file = Path(__main__.__file__)
    filename = str(main_file.name)
    day = filename.split("_")[1]
    input_filename = main_file.parent.joinpath("../", "data", f"day_{day}_input.txt")
    with open(input_filename, "r", encoding="utf-8") as fp:
        return fp.read().strip()


def run(solve_pt1: Callable, solve_pt2: Callable, tests: list[AocTestCase]):
    parser = argparse.ArgumentParser()
    parser.add_argument("command", choices=["run", "test"])
    args = parser.parse_args()
    match args.command:
        case "run":
            input_data = load_input()
            print(
                f"Part one: {solve_pt1(input_data)}",
            )
            print(
                f"Part two: {solve_pt2(input_data)}",
            )
        case "test":
            test_runner = unittest.TextTestRunner()
            suite = unittest.TestSuite(tests)
            test_runner.run(suite)
