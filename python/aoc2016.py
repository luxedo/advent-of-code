import __main__
import argparse
from pathlib import Path
import unittest


class AocTestCase(unittest.TestCase):
    def __init__(self, func, output, input_data):
        self.func = func
        self.output = output
        self.input_data = input_data
        super().__init__()

    def runTest(self):
        self.assertEqual(self.output, self.func(self.input_data))


def load_input():
    main_file = Path(__main__.__file__)
    filename = str(main_file.name)
    day = filename.split("_")[1]
    input_filename = main_file.parent.joinpath("../", "data", f"day_{day}_input.txt")
    with open(input_filename, "r", encoding="utf-8") as fp:
        return fp.read()


def run(solve_pt1, solve_pt2):
    input_data = load_input()
    print(
        f"Part one: {solve_pt1(input_data)}",
    )
    print(
        f"Part two: {solve_pt2(input_data)}",
    )


def test(tests):
    runner = unittest.TextTestRunner()
    suite = unittest.TestSuite(tests)
    runner.run(suite)


def parser(solve_pt1, solve_pt2, tests):
    p = argparse.ArgumentParser()
    p.add_argument("command", choices=["run", "test"])
    args = p.parse_args()
    if args.command == "run":
        run(solve_pt1, solve_pt2)
    else:
        test(tests)
