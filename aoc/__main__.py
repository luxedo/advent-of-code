"""
Advent of Code Runner

Fetches and runs code for the solutions.

Examples:
    python -m aoc --year 2022 --day 3 fetch python
    python -m aoc --year 2021 --day 13 run rust
"""
import argparse
import http.client
import json
import subprocess
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum
from html.parser import HTMLParser
from os import environ
from pathlib import Path
from string import punctuation
from textwrap import wrap
from typing import TextIO

ROOT_DIR = Path(__file__).parent.parent
HOST = "adventofcode.com"
ROUTE = "/{year}/day/{day}"
INPUT_URL = f"https://{HOST}{ROUTE}/input"


class RunMode(Enum):
    run = 0
    test = 1


@dataclass
class LanguageSpec:
    root: Path
    name: str
    template: Path
    template_destination_str: str
    extension: str
    test_template: Path | None
    test_template_destination_str: str | None
    test_extension: str | None
    project_base: Path | None
    run_command: list[str]
    test_command: list[str]
    destination: Callable[[int], Path] = field(init=False)
    input: Callable[[int, int], Path] = field(init=False)
    executable: Callable[[int, int], Path] = field(init=False)
    test_executable: Callable[[int, int], Path] = field(init=False)
    template_destination: Callable[[int], Path] = field(init=False)
    test_template_destination: Callable[[int], Path] = field(init=False)

    @classmethod
    def from_json(cls, root: Path):
        with open(root.joinpath("spec.json"), "r") as fp:
            return LanguageSpec.load(fp, root)

    @classmethod
    def load(cls, fp: TextIO, root: Path):
        spec = json.load(fp)
        project_base = (
            Path(root.joinpath(spec["project_base"]))
            if spec["project_base"] is not None
            else None
        )
        return LanguageSpec(
            root=root,
            name=root.name,
            template=spec["template"],
            template_destination_str=spec.get("template_destination", "."),
            extension=spec["extension"],
            test_template=spec.get("test_template"),
            test_template_destination_str=spec.get("test_template_destination", "."),
            test_extension=spec.get("test_extension"),
            project_base=project_base,
            run_command=spec["run_command"],
            test_command=spec["test_command"],
        )

    def __post_init__(self):
        self.name = self.root.name
        self.destination = lambda year: ROOT_DIR.joinpath(
            f"solutions/{year}", self.name
        )
        self.input = lambda year, day: ROOT_DIR.joinpath(
            f"solutions/{year}/data/day_{day:02}_input.txt"
        )
        self.filename = lambda day, extension: f"day_{day:02}_solution.{extension}"
        self.executable = lambda year, day: self.template_destination(year).joinpath(
            self.filename(day, self.extension)
        )
        self.test_executable = lambda year, day: self.test_template_destination(
            year
        ).joinpath(self.filename(day, self.test_extension))
        self.template_destination = lambda year: ROOT_DIR.joinpath(
            f"solutions/{year}/{self.name}/{self.template_destination_str}"
        )
        self.test_template_destination = lambda year: ROOT_DIR.joinpath(
            f"solutions/{year}/{self.name}/{self.test_template_destination_str}"
        )


def load_langs():
    return {
        d.name: LanguageSpec.from_json(d)
        for d in Path(f"{ROOT_DIR}/aoc/langs").iterdir()
        if d.is_dir() and not d.name.startswith("_")
    }


def aoc_main(spec: LanguageSpec, mode: RunMode, year: int, day: int):
    solution_not_found = f"No solutions found for lang: {spec.name}, year: {year}, day: {day}. Please fetch before running"
    if not spec.destination(year).is_dir():
        print(solution_not_found)
        return

    input_path = spec.input(year, day)
    if mode == RunMode.run and not input_path.is_file():
        print(
            f"Input not found for year {year}, day {day}."
            f"Please fetch it at {INPUT_URL.format(year=year, day=day)} "
            f"and save it to {str(input_path)}"
        )
        return

    solutions = [
        f
        for f in spec.template_destination(year).iterdir()
        if f.name.startswith(f"day_{day:02}_")
    ]
    command: list[str]
    match mode:
        case RunMode.run:
            command = spec.run_command
        case RunMode.test:
            command = spec.test_command
        case _:
            print("Mode {mode} not found")
            return

    executable = spec.executable(year, day)
    test_executable = spec.test_executable(year, day)
    destination = spec.destination(year)
    command = [
        c.format(
            executable=str(executable),
            test_executable=str(test_executable),
            destination=str(destination),
            module_nme=executable.name.removesuffix(f".{spec.extension}"),
            year=year,
            day=day,
        )
        for c in command
    ]

    match solutions:
        case [_]:
            subprocess.run(command, cwd=destination)
        case [_, _]:
            print(
                "More than one solution found for lang: {spec.name}, year: {year}, day: {day}"
            )
            return
        case _:
            print(solution_not_found)
            return


def fetch_url(route: str, cookie: str) -> str:
    conn = http.client.HTTPSConnection(HOST)
    conn.request("GET", route, headers={"Cookie": f"session={cookie}"})
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
    description_pt1 = p.text.strip()
    return "\n".join(
        "\n".join(wrap(line, width=100, initial_indent="  ", subsequent_indent="  "))
        for line in description_pt1.split("\n")
    )


def prepare_template(spec: LanguageSpec, year: int, day: int, cookie: str):
    def process_title(description_pt1: str, day: int) -> str:
        punc = "".join(p for p in punctuation if p != "_")
        return (
            description_pt1.split("\n")[0]
            .replace("-", "")
            .replace(":", "")
            .strip()
            .lower()
            .replace(" ", "_")
            .replace(f"day_{day}", f"day_{day:02}")
            .translate(str.maketrans("", "", punc))
        )

    def copy_replace_recursive(src: Path, dst: Path, year: int):
        if not dst.is_dir():
            dst.mkdir(parents=True, exist_ok=True)
        for f in src.iterdir():
            if f.is_dir():
                copy_replace_recursive(f, dst.joinpath(f.name), year)
            else:
                with f.open() as fp:
                    contents = fp.read().replace("{year}", str(year))
                with dst.joinpath(f.name).open("w") as fp:
                    fp.write(contents)

    def prepare_project(spec: LanguageSpec, year: int):
        project_dir = spec.destination(year)
        if not project_dir.is_dir():
            if spec.project_base is not None:
                print(f"Creating project base at {project_dir}...")
                copy_replace_recursive(spec.project_base, project_dir, year)
            template_destination = spec.template_destination(year)
            if not template_destination.is_dir():
                template_destination.mkdir(parents=True, exist_ok=True)

    def prepare_main_template(
        spec: LanguageSpec, year: int, day: int, title: str, description_pt1: str
    ):
        template = spec.root.joinpath(spec.template)
        filename = spec.executable(year, day)
        if filename.is_file():
            print(f"Main {filename} already exists. Skipping...")
            return
        print(f"Creating template for {filename}...")
        with open(template, "r", encoding="utf-8") as fp:
            full_url = f"https://{HOST}{route}"
            bp = (
                fp.read()
                .replace("{url}", full_url)
                .replace("{description_pt1}", description_pt1)
                .replace("{year}", f"{year}")
                .replace("{day}", f"{day:02}")
            )
        with open(filename, "w", encoding="utf-8") as fp:
            fp.write(bp)

    def prepare_test_template(spec: LanguageSpec, year: int, day: int):
        template = spec.root.joinpath(spec.test_template)
        filename = spec.test_executable(year, day)
        if filename.is_file():
            print(f"Test {filename} already exists. Skipping...")
            return
        print(f"Creating template for {filename}...")
        with open(template, "r", encoding="utf-8") as fp:
            full_url = f"https://{HOST}{route}"
            bp = (
                fp.read()
                .replace("{url}", full_url)
                .replace("{year}", f"{year}")
                .replace("{day}", f"{day:02}")
            )
        with open(filename, "w", encoding="utf-8") as fp:
            fp.write(bp)

    def prepare_input(spec: LanguageSpec, year: int, day: int):
        filename = spec.input(year, day)
        if filename.is_file():
            print(f"Input {filename} already exists. Skipping...")
            return

        print("Fetching input data: {filename}")
        problem_data = fetch_url(INPUT_URL.format(year=year, day=day), cookie)
        with open(filename, "w") as fp:
            fp.write(problem_data)

    route = ROUTE.format(year=year, day=day)
    body = fetch_url(route, cookie)
    description_pt1 = parse_body(body)
    title = process_title(description_pt1, day)
    prepare_project(spec, year)
    prepare_main_template(spec, year, day, title, description_pt1)

    prepare_input(spec, year, day)

    if spec.test_template:
        prepare_test_template(spec, year, day)


def update_main_template(spec: LanguageSpec, year: int, day: int, cookie: str):
    filename = spec.executable(year, day)
    if not filename.is_file():
        print(f"Main {filename} does not exists. Skipping...")
        return

    route = ROUTE.format(year=year, day=day)
    body = fetch_url(route, cookie)
    description_pt2 = parse_body(body)
    pt2_anchor = "--- Part Two ---"
    try:
        index = description_pt2.index(pt2_anchor)
        description_pt2 = "  " + description_pt2[index:]
    except ValueError:
        print("Could not find the second part of the problem. Are you there yet?")
        return
    with open(filename, "r+", encoding="utf-8") as fp:
        bp = fp.read().replace("{description_pt2}", description_pt2)
        if pt2_anchor not in bp:
            print(f"Updating template for {filename}...")
            fp.seek(0)
            fp.write(bp)
        else:
            print("The file already has the second part of the problem. Skipping...")


def run():
    class Command(Enum):
        run = 0
        test = 1
        fetch = 2
        fetch2 = 3

    lang_specs = load_langs()
    cookie = environ.get("AOC_SESSION_COOKIE")
    parser = argparse.ArgumentParser(
        description="Prepares the templates for the given day"
    )
    parser.add_argument("lang", choices=lang_specs.keys())
    parser.add_argument("command", choices=[c.name for c in Command])
    parser.add_argument("-d", "--day", required=True, type=int, help="AoC day")
    parser.add_argument("-y", "--year", required=True, type=int, help="AoC year")

    args = parser.parse_args()

    spec = lang_specs[args.lang]
    command = Command[args.command]
    match command:
        case Command.run | Command.test:
            aoc_main(spec, RunMode[args.command], args.year, args.day)
        case Command.fetch:
            prepare_template(spec, args.year, args.day, cookie)
        case Command.fetch2:
            update_main_template(spec, args.year, args.day, cookie)
        case _:
            raise ValueError(f"Command '{args.command}' not found.")


if __name__ == "__main__":
    run()
