"""
ElfScript Brigade

Advent Of Code 2016 Day 12
Python Solution

Day 12: Leonardo's Monorail

https://adventofcode.com/2016/day/12
"""

from abc import ABC, abstractmethod
from enum import Enum, auto
from dataclasses import dataclass, field
from typing import Generator, Self


class Register(Enum):
    a = auto()
    b = auto()
    c = auto()
    d = auto()


class Instruction(ABC):
    @abstractmethod
    def execute(self, computer: "NewComputer") -> "NewComputer": ...


@dataclass
class InsCpy(Instruction):
    src: int | Register
    dst: Register

    def execute(self, computer: "NewComputer") -> "NewComputer":
        match self.src:
            case Register():
                computer.set_register(self.dst, computer.load_register(self.src))
            case int():
                computer.set_register(self.dst, self.src)
        return computer


@dataclass
class InsInc(Instruction):
    reg: Register

    def execute(self, computer: "NewComputer") -> "NewComputer":
        computer.set_register(self.reg, computer.load_register(self.reg) + 1)
        return computer


@dataclass
class InsDec(Instruction):
    reg: Register

    def execute(self, computer: "NewComputer") -> "NewComputer":
        computer.set_register(self.reg, computer.load_register(self.reg) - 1)
        return computer


@dataclass
class InsJnzReg(Instruction):
    reg: Register
    skip: int

    def execute(self, computer: "NewComputer") -> "NewComputer":
        if computer.load_register(self.reg) != 0:
            computer.ip += self.skip - 1
        return computer


@dataclass
class InsJnzVal(Instruction):
    val: int
    skip: int

    def execute(self, computer: "NewComputer") -> "NewComputer":
        if self.val != 0:
            computer.ip += self.skip - 1
        return computer


@dataclass
class NewComputer:
    a: int = 0
    b: int = 0
    c: int = 0
    d: int = 0
    instructions: list[Instruction] = field(default_factory=list)
    ip: int = 0

    @staticmethod
    def parse_instructions(instructions: str) -> Generator[Instruction, None, None]:
        def parse_instruction(instruction: str) -> Instruction:
            match instruction.split():
                case ["cpy", src, dst] if src.isdigit():
                    return InsCpy(int(src), Register[dst])
                case ["cpy", src, dst]:
                    return InsCpy(Register[src], Register[dst])
                case ["inc", reg]:
                    return InsInc(Register[reg])
                case ["dec", reg]:
                    return InsDec(Register[reg])
                case ["jnz", src, skip] if src.isdigit():
                    return InsJnzVal(int(src), int(skip))
                case ["jnz", src, skip]:
                    return InsJnzReg(Register[src], int(skip))
                case _:
                    raise ValueError("Ops! Cannot parse instruction!")

        yield from (
            parse_instruction(instruction)
            for instruction in instructions.strip().split("\n")
        )

    @classmethod
    def run(cls, input_data: str, a: int = 0, b: int = 0, c: int = 0, d: int = 0) -> Self:
        computer = cls(a=a, b=b, c=c, d=d, instructions=list(cls.parse_instructions(input_data)))
        while computer.ip < len(computer.instructions):
            computer.execute_next()
        return computer

    def execute_next(self) -> "NewComputer":
        instruction = self.instructions[self.ip]
        return self.execute(instruction)

    def execute(self, instruction: Instruction) -> "NewComputer":
        computer = instruction.execute(self)
        computer.ip += 1
        return computer

    def load_register(self, reg: Register) -> int:
        return getattr(self, reg.name)

    def set_register(self, reg: Register, value: int):
        return setattr(self, reg.name, value)


def solve_pt1(input_data: str, args: list[str] | None = None) -> int:
    computer = NewComputer.run(input_data)
    return computer.a


def solve_pt2(input_data: str, args: list[str] | None = None) -> int:
    computer = NewComputer.run(input_data, c=1)
    return computer.a



if __name__ == "__main__":
    from esb.protocol import fireplace

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
