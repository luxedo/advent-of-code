"""
ElfScript Brigade

Advent Of Code 2016 Day 11
Python Solution

Day 11: Radioisotope Thermoelectric Generators

https://adventofcode.com/2016/day/11
"""

# 0. Microchips generate a radiation shield when paired correctly
# 1. Can only carry 2 items (microchips or RTGs)
# 2. The elevator needs at least one item to work (microchip or RTG)
# 3. The elevator always stops on each floor. So, items interact when this happens
# 4. Generators or microchips in the elevators can charge each other

from queue import PriorityQueue
from dataclasses import dataclass, field
from itertools import combinations, product
import re
from typing import Any, ClassVar, Self, Iterable


@dataclass(frozen=True)
class RTG:
    element: str

    def __repr__(self):
        return f"{self.element[0].upper()}G"

    def __lt__(self, other):
        return self.element < other.element


@dataclass(frozen=True)
class Microchip:
    element: str

    def __repr__(self):
        return f"{self.element[0].upper()}M"

    def __lt__(self, other):
        return self.element < other.element


Floors = tuple[frozenset[RTG | Microchip], ...]


@dataclass(frozen=True)
class Facility:
    floors: Floors
    elevator: int = 0
    previous: Self | None = field(compare=False, hash=False, default=None)
    floors_names: ClassVar = (
        "first",
        "second",
        "third",
        "fourth",
    )
    elevator_capacity: ClassVar = 2
    steps: int = field(compare=False, hash=False, default=0)

    @classmethod
    def new(
        cls,
        floors: Iterable[Iterable[RTG | Microchip]],
        elevator: int,
        previous: Self | None = None,
        steps: int = 0,
    ):
        return cls(
            tuple(frozenset(f) for f in floors), elevator=elevator, previous=previous, steps=steps
        )

    def __repr__(self) -> str:
        floors = [
            f'{f"  {key+1}  " if key != self.elevator else f"  {key+1} E"} - {sorted(tuple(value))}'
            for key, value in enumerate(self.floors)
        ]
        floors_str = '\n'.join(reversed(floors))
        return f"Facility:\n{floors_str}"

    def __hash__(self) -> int:
        h: list[Any] = [self.elevator]
        for floor in self.floors:
            lrs = len([r for r in floor if isinstance(r, RTG)])
            lms = len([r for r in floor if isinstance(r, Microchip)])
            h.append((lrs, lms))
        return hash(tuple(h))

    def __eq__(self, other):
        for i in range(len(self.floors)):
            ra, ma = list(zip(*[(1, 0) if isinstance(r, RTG) else (0, 1) for r in self.floors[i]] + [(0, 0)]))
            rb, mb = list(zip(*[(1, 0) if isinstance(r, RTG) else (0, 1) for r in other.floors[i]] + [(0, 0)]))
            if sum(ra) != sum(rb) or sum(ma) != sum(mb):
                return False
        return True

    def __lt__(self, other: Self):
        return self.distance < other.distance

    @property
    def distance(self):
        return self.steps + sum(len(f) * (len(self.floors) -i - 1) for i, f in enumerate(self.floors))

    @classmethod
    def parse(cls, input_data: str) -> Self:
        floors: list[list[RTG | Microchip]] = [list() for _ in range(len(cls.floors_names))]
        for line in input_data.split("\n"):
            floor_idx, items = cls.parse_line(line)
            floors[floor_idx].extend(items)
        return cls(tuple(frozenset(items) for items in floors))

    @classmethod
    def parse_line(cls, line: str) -> tuple[int, frozenset[RTG | Microchip]]:
        header = re.search(
            f"The ({'|'.join(cls.floors_names)}) floor contains (.*)", line
        )
        match header:
            case None:
                raise ValueError("Oh no! No match!")
            case match:
                floor_index = cls.floors_names.index(match.group(1))
                rest = match.group(2)

        items: frozenset[RTG | Microchip]
        if rest == "nothing relevant.":
            items = frozenset()
        else:
            items = frozenset(cls.parse_items(rest))
        return floor_index, items

    @classmethod
    def parse_items(cls, line: str) -> frozenset[RTG | Microchip]:
        return frozenset(cls.parse_item(item) for item in re.split(",? and |, ", line))

    @classmethod
    def parse_item(cls, line: str) -> RTG | Microchip:
        micro_re = "^an? (.*)-compatible microchip.?$"
        gen_re = "^an? (.*) generator.?$"

        if (m := re.match(micro_re, line)) is not None:
            return Microchip(m.group(1))

        elif (m := re.match(gen_re, line)) is not None:
            return RTG(m.group(1))

        raise ValueError("NOOOO ")

    @staticmethod
    def is_allowed(c1: Microchip | RTG, c2: Microchip | RTG):
        match c1, c2:
            case (Microchip(), Microchip()) | (RTG(), RTG()):
                return True
            case (Microchip(element=e1), RTG(element=e2)):
                return e1 == e2
            case (RTG(element=e1), Microchip(element=e2)):
                return e1 == e2
            case _:
                raise ValueError("Unexpected state!")

    def possible_states(self) -> list[Self]:
        allowed_floors = set([self.elevator - 1, self.elevator + 1]) - set(
            [-1, len(self.floors)]
        )
        current_floor = self.floors[self.elevator]
        moves = list((f,) for f in current_floor) + list(
            (c1, c2)
            for (c1, c2) in combinations(current_floor, self.elevator_capacity)
            if self.is_allowed(c1, c2)
        )
        states = []
        for elevator_position, items in product(allowed_floors, moves):
            new_facility = self.move(elevator_position, items)
            if new_facility.is_valid():
                states.append(new_facility)
        return states

    def move(self, elevator_position, items):
        new_floors = list(self.floors)
        new_floors[self.elevator] = self.floors[self.elevator] - set(items)
        new_floors[elevator_position] = self.floors[elevator_position] | set(items)
        return self.new(new_floors, elevator=elevator_position, previous=self, steps=self.steps+1)

    def is_valid(self) -> bool:
        for floor in self.floors:
            rtgs = set(item.element for item in floor if isinstance(item, RTG))
            micros = set(item.element for item in floor if isinstance(item, Microchip))
            if len(micros) == 0:
                continue
            if len(rtgs) > 0 and len(micros - rtgs) > 0:
                return False
            # if len(rtgs) > 0 and not any(Microchip(r.element) in micros for r in rtgs):
            #     return False
        return True

    def done(self):
        for idx, floor in enumerate(self.floors):
            if idx == (len(self.floors) - 1):
                continue
            if len(floor) != 0:
                return False
        return True


def solve_pt1(input_data: str, args: list[str] | None = None) -> int:
    initial_state = Facility.parse(input_data)
    queue: PriorityQueue[Facility] = PriorityQueue()
    queue.put(initial_state)
    seen = set([initial_state])
    try:
        while state := queue.get():
            if state.done():
                break
            next_states = set(state.possible_states()) - seen
            seen |= next_states
            for next in next_states:
                queue.put(next)
    except IndexError:
        raise ValueError("Could not find solution")

    chain = [state]
    while (state := state.previous) is not None:
        chain.append(state)
    return len(chain) - 1


def solve_pt2(input_data: str, args: list[str] | None = None) -> int:
    input_data += "\n"
    input_data += "The first floor contains an elerium generator and an elerium-compatible microchip.\n"
    input_data += "The first floor contains a dilithium generator and a dilithium-compatible microchip."
    return solve_pt1(input_data, args)


if __name__ == "__main__":
    from esb.protocol import fireplace

    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    fireplace.v1_run(solve_pt1, solve_pt2)
