defmodule Year2023Day01 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 01
  Python Solution

  Day 1: Trebuchet?!

  https://adventofcode.com/2023/day/1
  """
  import EsbFireplace

  @table %{
    "one" => "1",
    "two" => "2",
    "three" => "3",
    "four" => "4",
    "five" => "5",
    "six" => "6",
    "seven" => "7",
    "eight" => "8",
    "nine" => "9"
  }

  @digits Map.values(@table)

  def solve_pt1(input, _args) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      Regex.replace(~r/[^\d]/, line, "", global: true)
      |> (fn number ->
            (String.at(number, 0) <> String.at(number, -1)) |> String.to_integer()
          end).()
    end)
    |> Enum.sum()
  end

  def solve_pt2(input, _args) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      digits =
        Enum.map(0..(String.length(line) - 1), fn i ->
          substring = String.slice(line, i..-1//1)

          case String.at(substring, 0) do
            n when n in @digits ->
              n

            _ ->
              Enum.find(@table, {nil, nil}, fn {key, _} ->
                String.starts_with?(substring, key)
              end)
              |> elem(1)
          end
        end)
        |> Enum.filter(fn i -> !is_nil(i) end)

      (List.first(digits) <> List.last(digits)) |> String.to_integer()
    end)
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end

