"""
Advent of Code Runner

Fetches and runs code for the solutions.

Examples:
    python -m aoc --year 2022 --day 3 fetch python
    python -m aoc --year 2021 --day 13 run rust
"""
import argparse
import http.client
import subprocess
from html.parser import HTMLParser
from pathlib import Path
from string import punctuation

ROOT_DIR = Path(__file__).parent.parent
PYTHON_ROOT = f"{ROOT_DIR}/{{year}}/python/"
PYTHON_TEMPLATE = f"{ROOT_DIR}/aoc/langs/python/template.py"
RUST_ROOT = f"{ROOT_DIR}/{{year}}/rust/"
RUST_TEMPLATE = f"{ROOT_DIR}/aoc/langs/rust/src/template.rs"
INPUT_FILE = "{year}/data/day_{day:02}_input.txt"
HOST = "adventofcode.com"
ROUTE = "/{year}/day/{day}"


def aoc_main(mode: str, lang: str, year: int, day: int):
    match lang:
        case "python":
            python_root = PYTHON_ROOT.format(year=year)
            if not Path(python_root).is_dir():
                print("Python solutions are not set yet")
                return
            filename = [
                f for f in Path(python_root).iterdir() if f"day_{day:02}_" in f.name
            ]
            if len(filename) != 0:
                subprocess.run(["python", filename[0], mode])
            else:
                print(f"Could not find solution for day {day}")
        case "rust":
            rust_root = RUST_ROOT.format(year=year)
            if not Path(rust_root).is_dir():
                print("Rust solutions are not set yet")
                return
            filename = [
                f
                for f in Path(rust_root).joinpath("src", "bin").iterdir()
                if f"day_{day:02}_" in f.name
            ]
            if len(filename) != 0:
                subprocess.run(
                    [
                        "cargo",
                        "watch",
                        "--workdir",
                        rust_root,
                        "--exec",
                        f"{mode} --bin {filename[0].name.removesuffix('.rs')}",
                    ]
                )
            else:
                print(f"Could not find solution for day {day}")
        case _:
            raise ValueError("Could not find language '{lang}'")
    input_path = Path(INPUT_FILE.format(year=year, day=day))
    if mode == "run" and not input_path.is_file():
        url = f"https://{HOST}{ROUTE.format(year=year, day=day)}/input"
        print(
            f"Input not found for year {year}, day {day}. Please fetch it at {url} "
            f"and save it to {str(input_path)}"
        )


def prepare_template(lang: str, year: int, day: int):
    route = ROUTE.format(year=year, day=day)

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
        punc = "".join(p for p in punctuation if p != "_")
        return (
            description.split("\n")[0]
            .replace("-", "")
            .replace(":", "")
            .strip()
            .lower()
            .replace(" ", "_")
            .replace(f"day_{day}", f"day_{day:02}")
            .translate(str.maketrans("", "", punc))
        )

    def boilerplate(lang: str, year: int, day: int, title: str, description: str):
        full_url = f"https://{HOST}{route}"
        match lang:
            case "python":
                filename = Path(PYTHON_ROOT.format(year=year)).joinpath(f"{title}.py")
                template = Path(PYTHON_TEMPLATE)
            case "rust":
                filename = Path(RUST_ROOT.format(year=year)).joinpath(
                    "src", "bin", f"{title}.rs"
                )
                template = Path(RUST_TEMPLATE)
                description = "\n".join(
                    [" * " + line for line in description.split("\n")]
                )
            case _:
                raise ValueError(f"Language {lang} not found")

        if filename.is_file():
            print(f"{filename} already exists. Skipping")
        else:
            print(f"Creating boilerplate for {filename}...")
            with open(template, "r", encoding="utf-8") as fp:
                bp = (
                    fp.read()
                    .replace("{url}", full_url)
                    .replace("{description}", description)
                    .replace("{year}", f"{year}")
                    .replace("{day}", f"{day:02}")
                )
            with open(filename, "w", encoding="utf-8") as fp:
                fp.write(bp)

    body = fetch_url(route)
    description = parse_body(body)
    title = process_title(description, day)

    boilerplate(lang, year, day, title, description)


def run():
    parser = argparse.ArgumentParser(
        description="Prepares the templates for the given day"
    )
    parser.add_argument("command", choices=["run", "test", "fetch"])
    parser.add_argument("lang", choices=["python", "rust"])
    parser.add_argument("-d", "--day", required=True, type=int, help="AoC day")
    parser.add_argument("-y", "--year", required=True, type=int, help="AoC year")

    args = parser.parse_args()
    match args.command:
        case "run" | "test":
            aoc_main(args.command, args.lang, args.year, args.day)
        case "fetch":
            prepare_template(args.lang, args.year, args.day)
        case _:
            raise ValueError(f"Command '{args.command}' not found.")


if __name__ == "__main__":
    run()
