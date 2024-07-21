"""
ElfScript Brigade

Advent Of Code 2016 Day 10
Python Solution

Day 10: Balance Bots

https://adventofcode.com/2016/day/10
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from math import prod
from re import match
from typing import Self


@dataclass
class Instruction:
    @classmethod
    def parse_line(cls, line: str) -> Self:
        match line.split():
            case ["bot", *_]:
                return GiveInstruction.parse_line(line)
            case ["value", *_]:
                return ValueInstruction.parse_line(line)
            case _:
                raise ValueError(f"Could not parse line {line}")


class GiveDestination(Enum):
    BOT = auto()
    OUTPUT = auto()

    @classmethod
    def from_str(cls, string) -> Self:
        match string:
            case "bot":
                return cls.BOT
            case "output":
                return cls.OUTPUT


@dataclass
class GiveInstruction(Instruction):
    bot_id: int
    low_dest_type: GiveDestination
    low_dest: int
    high_dest_type: GiveDestination
    high_dest: int

    @classmethod
    def parse_line(cls, line: str) -> Self:
        m = match(
            "^bot ([\\d]+) gives low to (bot|output) ([\\d]+) and high to (bot|output) ([\\d]+)$",
            line,
        )
        bot_id, low_dest_type_str, low_dest, high_dest_type_str, high_dest = (
            int(m.group(1)),
            m.group(2),
            int(m.group(3)),
            m.group(4),
            int(m.group(5)),
        )
        low_dest_type = GiveDestination.from_str(low_dest_type_str)
        high_dest_type = GiveDestination.from_str(high_dest_type_str)
        return cls(bot_id, low_dest_type, low_dest, high_dest_type, high_dest)


@dataclass
class ValueInstruction(Instruction):
    value: int
    bot_id: int

    @classmethod
    def parse_line(cls, line: str) -> Self:
        m = match("^value ([\\d]+) goes to bot ([\\d]+)$", line)
        value, bot_id = int(m.group(1)), int(m.group(2))
        return cls(value, bot_id)


@dataclass
class Bot:
    id: int
    low: int | None = field(default=None, hash=False)
    high: int | None = field(default=None, hash=False)

    def clear(self):
        self.low = self.high = None

    def give(self, value: int):
        match self:
            case Bot(low=None, high=None):
                self.low = value
            case Bot(low=int(value2), high=None) | Bot(low=None, high=int(value2)):
                if value > value2:
                    self.low = value2
                    self.high = value
                else:
                    self.low = value
                    self.high = value2
            case Bot(low=int(_), high=int(_)):
                raise ValueError(f"Bot {self.id} is already full")
            case _:
                raise ValueError("Shouldn't reach here!")


@dataclass
class Output:
    id: int
    values: list[int] = field(default_factory=list)

    def append(self, value: int):
        self.values.append(value)


@dataclass(frozen=True)
class Comparison:
    bot_id: int
    low: int
    high: int


@dataclass
class Factory:
    bots: dict[int, Bot] = field(default_factory=dict)
    outputs: dict[int, Output] = field(default_factory=dict)
    comparisons: set[Comparison] = field(default_factory=set)

    def run(self, instructions) -> set[Comparison]:
        preload = [i for i in instructions if isinstance(i, ValueInstruction)]
        for value in preload:
            self.execute(value)

        moves = {
            give.bot_id: give
            for give in instructions
            if isinstance(give, GiveInstruction)
        }
        while (bot_id := self.has_moves()) is not None:
            cmp = self.execute(moves[bot_id])
            if cmp is not None:
                self.comparisons.add(cmp)
        return self.comparisons

    def execute(self, instruction) -> Comparison | None:
        match instruction:
            case ValueInstruction(value=value, bot_id=bot_id):
                self.give_bot(bot_id, value)
            case GiveInstruction(
                bot_id, low_dest_type, low_dest, high_dest_type, high_dest
            ):
                bot = self.get_bot(bot_id)
                low, high = bot.low, bot.high
                bot.clear()
                match low_dest_type:
                    case GiveDestination.BOT:
                        self.give_bot(low_dest, low)
                    case GiveDestination.OUTPUT:
                        self.give_output(low_dest, low)

                match high_dest_type:
                    case GiveDestination.BOT:
                        self.give_bot(high_dest, high)
                    case GiveDestination.OUTPUT:
                        self.give_output(high_dest, high)
                return Comparison(bot_id, low, high)

    def has_moves(self) -> int | None:
        for bot in self.bots.values():
            match bot:
                case Bot(low=int(_), high=int(_)):
                    return bot.id
        return None

    def give_bot(self, bot_id, value):
        self.get_bot(bot_id).give(value)

    def clear_bot(self, bot_id):
        self.get_bot(bot_id).clear()

    def get_bot(self, bot_id):
        if bot_id not in self.bots:
            self.bots[bot_id] = Bot(bot_id)
        return self.bots[bot_id]

    def get_output(self, output_id):
        if output_id not in self.outputs:
            self.outputs[output_id] = Output(output_id)
        return self.outputs[output_id]

    def give_output(self, output_id, value):
        self.get_output(output_id).append(value)


def parse(input_data: str):
    return [Instruction.parse_line(line) for line in input_data.split("\n")]


def solve_pt1(input_data: str, args: list[str] | None = None) -> int:
    instructions = parse(input_data)
    factory = Factory()
    comparisons = factory.run(instructions)
    cmp_high, cmp_low = (int(a) for a in args) if args is not None else (61, 17)
    match [cmp for cmp in comparisons if cmp.low == cmp_low and cmp.high == cmp_high]:
        case [cmp]:
            return cmp.bot_id
        case _:
            raise ValueError(f"Could not find comparison for {cmp_high}, {cmp_low}")


def solve_pt2(input_data: str, args: list[str] | None = None) -> int:
    instructions = parse(input_data)
    factory = Factory()
    factory.run(instructions)
    outputs = factory.outputs
    return prod([*outputs[0].values, *outputs[1].values, *outputs[2].values])


if __name__ == "__main__":
    from esb.protocol import fireplace

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
