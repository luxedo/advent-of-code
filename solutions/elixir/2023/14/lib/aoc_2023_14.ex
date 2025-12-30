defmodule Year2023Day14 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 14
  Elixir Solution

  Day 14: Parabolic Reflector Dish

  https://adventofcode.com/2023/day/14
  """
  import EsbFireplace

  def parse(input) do
    String.split(input, "\n", trim: true) |> Enum.map(&String.graphemes/1)
  end

  def transpose(rows) do
    rows
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def tilt(rocks, :east) do
    Enum.map(rocks, fn line ->
      Enum.reduce(line, [[], []], fn r, [rounds, acc] ->
        case r do
          "O" -> [[r | rounds], acc]
          "." -> [rounds, [r | acc]]
          "#" -> [[], [r | rounds ++ acc]]
        end
      end)
      |> Enum.concat()
      |> Enum.reverse()
    end)
  end

  def tilt(rocks, :west) do
    Enum.map(rocks, &Enum.reverse/1) |> tilt(:east) |> Enum.map(&Enum.reverse/1)
  end

  def tilt(rocks, :south) do
    transpose(rocks) |> tilt(:east) |> transpose
  end

  def tilt(rocks, :north) do
    transpose(rocks) |> tilt(:west) |> transpose
  end

  def beam_load(rocks) do
    Enum.reverse(rocks)
    |> Enum.with_index()
    |> Enum.map(fn {line, i} ->
      Enum.count(line, &(&1 == "O")) * (i + 1)
    end)
    |> Enum.sum()
  end

  def cycle(rocks) do
    tilt(rocks, :north) |> tilt(:west) |> tilt(:south) |> tilt(:east)
  end

  def show(rocks) do
    Enum.join(rocks, "\n") |> IO.puts()
  end

  def solve_pt1(input_data, _args) do
    parse(input_data)
    |> tilt(:north)
    |> beam_load
  end

  def solve_pt2(input_data, _args) do
    rocks = parse(input_data)
    cycles = 1_000_000_000

    {loop, offset, rocks} =
      Enum.reduce_while(0..cycles, [rocks], fn i, acc = [r | _] ->
        newr = cycle(r)

        if offset = Enum.find_index(acc, &(&1 == newr)) do
          offset = i - offset
          {:halt, {i - offset + 1, offset, r}}
        else
          {:cont, [newr | acc]}
        end
      end)

    cycles = Kernel.rem(cycles - offset, loop) + 1

    Enum.reduce(1..cycles, rocks, fn _, r -> cycle(r) end) |> beam_load
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
