defmodule Year2023Day02 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 02
  Elixir Solution

  Day 2: Cube Conundrum

  https://adventofcode.com/2023/day/2
  """
  import EsbFireplace

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      [game, sets] = String.split(String.replace_prefix(line, "Game ", ""), ": ")

      cubes =
        String.split(sets, "; ")
        |> Enum.map(fn set ->
          String.split(set, ", ")
          |> Enum.map(fn cubes -> String.split(cubes, " ") end)
          |> Enum.map(fn [v, k] -> {k, String.to_integer(v)} end)
          |> Map.new()
          |> (fn m ->
                [Map.get(m, "red", 0), Map.get(m, "green", 0), Map.get(m, "blue", 0)]
              end).()
        end)

      {String.to_integer(game), cubes}
    end)
  end

  def solve_pt1(input_data, _args) do
    maxCubes = [12, 13, 14]

    parse(input_data)
    |> Enum.map(fn {game, cubes} ->
      {game,
       Enum.all?(
         Enum.map(cubes, fn cube ->
           if Enum.zip(cube, maxCubes) |> Enum.map(fn {c, mc} -> c > mc end) |> Enum.any?(),
             do: false,
             else: true
         end)
       )}
    end)
    |> Enum.filter(fn {_, p} -> p end)
    |> Enum.map(fn {g, _} -> g end)
    |> Enum.sum()
  end

  def solve_pt2(input_data, _args) do
    parse(input_data)
    |> Enum.map(fn {_, cubes} ->
      Enum.reduce(cubes, [0, 0, 0], fn cube, acc ->
        Enum.zip(cube, acc)
        |> Enum.map(fn {prev, cur} -> if cur > prev, do: cur, else: prev end)
      end)
      |> Enum.reduce(1, fn color, acc -> color * acc end)
    end)
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
