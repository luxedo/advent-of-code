"""
  Advent Of Code 2017 - day 01
  https://adventofcode.com/2017/day/1

    --- Day 1: Inverse Captcha ---
  The night before Christmas, one of Santa's Elves calls you in a panic. "The printer's broken! We
  can't print the Naughty or Nice List!" By the time you make it to sub-basement 17, there are only
  a few minutes until midnight. "We have a big problem," she says; "there must be almost fifty bugs
  in this system, but nothing else can print The List. Stand in this square, quick! There's no time
  to explain; if you can convince them to pay you in stars, you'll be able to--" She pulls a lever
  and the world goes blurry.

  When your eyes can focus again, everything seems a lot more pixelated than before. She must have
  sent you inside the computer! You check the system clock: 25 milliseconds until midnight. With
  that much time, you should be able to collect all fifty stars by December 25th.

  Collect stars by solving puzzles.  Two puzzles will be made available on each day millisecond in
  the Advent calendar; the second puzzle is unlocked when you complete the first.  Each puzzle
  grants one star. Good luck!

  You're standing in a room with "digitization quarantine" written in LEDs along one wall. The only
  door is locked, but it includes a small interface. "Restricted Area - Strictly No Digitized Users
  Allowed."

  It goes on to explain that you may only leave by solving a captcha to prove you're not a human.
  Apparently, you only get one millisecond to solve the captcha: too fast for a normal human, but it
  feels like hours to you.

  The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of
  all digits that match the next digit in the list. The list is circular, so the digit after the
  last digit is the first digit in the list.

  For example:

  1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the
  third digit (2) matches the fourth digit.
  1111 produces 4 because each digit (all 1) matches the next.
  1234 produces 0 because no digit matches the next.
  91212129 produces 9 because the only digit that matches the next one is the last digit, 9.


  What is the solution to your captcha?
"""

from aoc.langs.python import runner


def parse_input(input_data: str) -> list[int]:
    return [int(i) for i in list(input_data)]


def rotate_list(lst: list[int], n: int) -> list[int]:
    knife = len(lst) - n
    return lst[knife:] + lst[:knife]


def sum_matching(l1: list[int], l2: list[int]) -> int:
    return sum([i for i, j in zip(l1, l2) if i == j])


def solve_pt1(input_data: str) -> int:
    captcha = parse_input(input_data)
    # solution_pt1: ???
    return sum_matching(captcha, rotate_list(captcha, 1))


def solve_pt2(input_data: str) -> int:
    captcha = parse_input(input_data)
    # solution_pt2: ???
    return sum_matching(captcha, rotate_list(captcha, len(captcha) // 2))


if __name__ == "__main__":
    tests = [
        runner.AocTestCase(func=solve_pt1, output=3, input_data="1122"),
        runner.AocTestCase(func=solve_pt1, output=4, input_data="1111"),
        runner.AocTestCase(func=solve_pt1, output=0, input_data="1234"),
        runner.AocTestCase(func=solve_pt1, output=9, input_data="91212129"),
        runner.AocTestCase(func=solve_pt2, output=6, input_data="1212"),
        runner.AocTestCase(func=solve_pt2, output=0, input_data="1221"),
        runner.AocTestCase(func=solve_pt2, output=4, input_data="123425"),
        runner.AocTestCase(func=solve_pt2, output=12, input_data="123123"),
        runner.AocTestCase(func=solve_pt2, output=4, input_data="12131415"),
    ]
    runner.run(solve_pt1, solve_pt2, tests)
