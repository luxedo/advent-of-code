defmodule Year2023Day06 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 06
  Elixir Solution

  Day 6: Wait For It

  https://adventofcode.com/2023/day/6
  """
  import EsbFireplace

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      String.split(line, " ", trim: true) |> List.delete_at(0) |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.zip()
  end

  def find_roots(eqs) when is_list(eqs) do
    Enum.map(eqs, &find_roots/1)
  end

  def find_roots({b, c}) do
    {a, b} = {1, -b}
    d = (b ** 2 - 4 * a * c) ** 0.5
    {(-b - d) / (2 * a), (-b + d) / (2 * a)}
  end

  def integers_in_interval(intervals) when is_list(intervals) do
    Enum.map(intervals, &integers_in_interval/1)
  end

  def integers_in_interval({s, e}) do
    trunc(Float.ceil(e - 1) - Float.floor(s + 1) + 1)
  end

  def concat_intervals(intervals) when is_list(intervals) do
    {time, distance} =
      Enum.reduce(intervals, {"", ""}, fn {b, c}, {ab, ac} ->
        {ab <> Integer.to_string(b), ac <> Integer.to_string(c)}
      end)

    {String.to_integer(time), String.to_integer(distance)}
  end

  def solve_pt1(input_data, _args) do
    parse(input_data) |> find_roots |> integers_in_interval |> Enum.product()
  end

  def solve_pt2(input_data, _args) do
    parse(input_data) |> concat_intervals |> find_roots |> integers_in_interval
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
