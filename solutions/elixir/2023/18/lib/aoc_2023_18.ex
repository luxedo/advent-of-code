defmodule Year2023Day18 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 18
  Elixir Solution

  Day 18: Lavaduct Lagoon

  https://adventofcode.com/2023/day/18
  """
  import EsbFireplace

  require Integer

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      [side, steps, color] = String.split(line, " ")
      steps = String.to_integer(steps)

      color = String.trim_leading(color, "(#") |> String.trim_trailing(")")

      side2 =
        case String.last(color) do
          "0" -> "R"
          "1" -> "D"
          "2" -> "L"
          "3" -> "U"
        end

      {steps2, ""} = String.slice(color, 0..4) |> Integer.parse(16)

      {side, steps, {side2, steps2}}
    end)
  end

  defmodule Digger do
    defstruct pos: {0, 0}

    def move(%Digger{pos: {y, x}}, side, steps) do
      {dy, dx} =
        case side do
          "R" -> {0, steps}
          "D" -> {steps, 0}
          "L" -> {0, -steps}
          "U" -> {-steps, 0}
        end

      %Digger{pos: {y + dy, x + dx}}
    end
  end

  defmodule Trench do
    defstruct [:sparse, height: nil, width: nil]

    def dig_plans(plans) do
      {_, history} =
        Enum.reduce(plans, {%Digger{}, []}, fn {side, steps}, {digger, history} ->
          digger = Digger.move(digger, side, steps)
          {digger, [digger.pos | history]}
        end)

      center(history)
    end

    def center(sparse) do
      miny = Enum.map(sparse, &elem(&1, 0)) |> Enum.min()
      maxy = Enum.map(sparse, &elem(&1, 0)) |> Enum.max()
      minx = Enum.map(sparse, &elem(&1, 1)) |> Enum.min()
      maxx = Enum.map(sparse, &elem(&1, 1)) |> Enum.max()

      sparse = Enum.map(sparse, fn {y, x} -> {y - miny, x - minx} end)

      %Trench{sparse: sparse, height: maxy - miny, width: maxx - minx}
    end

    def area(%Trench{sparse: sparse}) do
      {area2, perimeter, _} =
        Enum.reduce(sparse, {0, 0, List.last(sparse)}, fn {y, x}, {area, perimeter, {py, px}} ->
          area = area + (py + y) * (px - x)
          perimeter = perimeter + abs(x - px) + abs(y - py)
          {area, perimeter, {y, x}}
        end)

      trunc(Kernel.abs(area2 / 2) + perimeter / 2 + 1)
    end
  end

  defp solve_pt1(input_data, _args) do
    plans = parse(input_data) |> Enum.map(fn {d, s, _} -> {d, s} end)
    Trench.dig_plans(plans) |> Trench.area()
  end

  defp solve_pt2(input_data, _args) do
    plans = parse(input_data) |> Enum.map(fn {_, _, p} -> p end)
    Trench.dig_plans(plans) |> Trench.area()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
