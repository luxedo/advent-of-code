defmodule Year2023Day13 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 13
  Elixir Solution

  Day 13: Point of Incidence

  https://adventofcode.com/2023/day/13
  """
  import EsbFireplace

  require Integer

  def parse(input) do
    String.split(input, "\n\n", trim: true)
    |> Enum.map(fn pattern ->
      String.split(pattern, "\n", trim: true) |> Enum.map(&String.graphemes/1)
    end)
  end

  def transpose(rows) do
    rows
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def is_reflection?([]), do: true

  def is_reflection?(pattern) do
    if List.first(pattern) == List.last(pattern) do
      is_reflection?(List.delete_at(pattern, 0) |> List.delete_at(-1))
    else
      false
    end
  end

  def find_reflection(pattern) do
    height = Kernel.length(pattern)

    case Enum.to_list(1..(height - 1))
         |> Enum.find(fn y ->
           width = Enum.min([y, height - y])
           {top, bottom} = {y + width - 1, y - width}
           Enum.slice(pattern, bottom..top) |> is_reflection?
         end) do
      nil -> {:err}
      y -> {:ok, y}
    end
  end

  def possible_smudges([]), do: 0
  def possible_smudges([_]), do: 0

  def possible_smudges(pattern) do
    smudges =
      Enum.zip(List.first(pattern), List.last(pattern))
      |> Enum.map(fn {a, b} -> (a == b && 0) || 1 end)
      |> Enum.sum()

    next_smugdes = possible_smudges(List.delete_at(pattern, 0) |> List.delete_at(-1))
    smudges + next_smugdes
  end

  def find_smudges([]), do: []

  def find_smudges(pattern) do
    (Enum.zip(List.first(pattern), List.last(pattern))
     |> Enum.with_index()
     |> Enum.map(fn {{a, b}, x} ->
       {a, b, {0, x}}
       (a !== b && {0, x}) || nil
     end)
     |> Enum.filter(&(&1 !== nil))) ++
      (find_smudges(
         List.delete_at(pattern, 0)
         |> List.delete_at(-1)
       )
       |> Enum.map(fn {y, x} -> {y + 1, x} end))
  end

  def repair_smugde(pattern) do
    height = Kernel.length(pattern)

    case Enum.to_list(1..(height - 1))
         |> Enum.find(fn y ->
           width = Enum.min([y, height - y])
           {top, bottom} = {y + width - 1, y - width}

           case Enum.slice(pattern, bottom..top) |> find_smudges do
             [{_, _}] -> true
             _ -> false
           end
         end) do
      nil -> {:err}
      y -> {:ok, y}
    end
  end

  def solve_pt1(input_data, _args) do
    patterns = parse(input_data)

    Enum.map(patterns, fn p ->
      case find_reflection(p) do
        {:ok, i} ->
          100 * i

        {:err} ->
          {:ok, i} = transpose(p) |> find_reflection()
          i
      end
    end)
    |> Enum.sum()
  end

  def solve_pt2(input_data, _args) do
    patterns = parse(input_data)

    Enum.map(patterns, fn p ->
      case repair_smugde(p) do
        {:ok, i} ->
          100 * i

        {:err} ->
          {:ok, i} = transpose(p) |> repair_smugde()
          i
      end
    end)
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
