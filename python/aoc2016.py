import argparse
import http.client
import unittest
from collections.abc import Callable
from html.parser import HTMLParser
from pathlib import Path
from subprocess import run
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


def aoc_main(mode: str, year: int, day: int):
    filename = [f for f in Path("python/").iterdir() if f"day_{day:02}_" in f.name]
    if len(filename) == 0:
        print(f"Could not find solution for day {day}")
        return
    run(["python", filename[0], mode])


def runner(solve_pt1: Callable, solve_pt2: Callable, tests: list[AocTestCase]):
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


def prepare_template(year: int, day: int):
    HOST = "adventofcode.com"
    ROUTE = f"/{year}/day/{day}"

    def fetch_url(route: str) -> str:
        conn = http.client.HTTPSConnection(HOST)
        conn.request("GET", route)
        res = conn.getresponse()
        if res.status != 200:
            raise ValueError("Could not connect!")
        return res.read().decode("utf-8")

    def parse_body(body: str) -> str:
        class AoCHTMLParser(HTMLParser):
            def __init__(self, *args, **kwargs):
                super().__init__(*args, **kwargs)
                self.tags = []
                self.text = ""

            def handle_starttag(self, tag, attrs):
                self.tags.append(tag)
                if tag == "p":
                    self.text += "\n"

            def handle_endtag(self, tag):
                self.tags.pop()

            def handle_data(self, data):
                if "article" in self.tags:
                    self.text += data

        p = AoCHTMLParser()
        p.feed(body)
        return p.text.strip()

    def process_title(description: str, day: int) -> str:
        return (
            description.split("\n")[0]
            .replace("-", "")
            .replace(":", "")
            .strip()
            .lower()
            .replace(" ", "_")
            .replace(f"day_{day}", f"day_{day:02}")
        )

    def boilerplate(lang: str, year: int, day: int, title: str, description: str):
        full_url = f"https://{HOST}{ROUTE}"
        match lang:
            case "python":
                filename = Path(f"python/{title}.py")
                template = Path("python/problem_template.py")
            case "rust":
                filename = Path(f"rust/src/bin/{title}.rs")
                template = Path("rust/src/problem_template.rs")
                description = "\n".join(
                    [" * " + line for line in description.split("\n")]
                )
            case _:
                raise ValueError(f"Lang {lang} not found")

        if filename.is_file():
            print(f"{filename} already exists. Skipping")
        else:
            print(f"Creating boilerplate for {filename}...")
            with open(template, "r", encoding="utf-8") as fp:
                boilerplate = (
                    fp.read()
                    .replace("{url}", full_url)
                    .replace("{description}", description)
                    .replace("{day}", f"{day:02}")
                )
            with open(filename, "w", encoding="utf-8") as fp:
                fp.write(boilerplate)

    body = fetch_url(ROUTE)
    description = parse_body(body)
    title = process_title(description, day)

    boilerplate("python", year, day, title, description)

    boilerplate("rust", year, day, title, description)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Prepares the templates for the given day"
    )
    parser.add_argument("command", choices=["run", "test", "fetch"])
    parser.add_argument("-d", "--day", required=True, type=int, help="AoC day")
    parser.add_argument("-y", "--year", required=True, type=int, help="AoC year")

    args = parser.parse_args()
    match args.command:
        case "run" | "test":
            aoc_main(args.command, args.year, args.day)
        case "fetch":
            prepare_template(args.year, args.day)
