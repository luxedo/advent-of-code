defmodule Year2023Day09 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 09
  Elixir Solution

  Day 9: Mirage Maintenance

  https://adventofcode.com/2023/day/9
  """
  import EsbFireplace

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(&(String.split(&1) |> Enum.map(fn x -> String.to_integer(x) end)))
  end

  def find_sequence(seq) when is_list(seq) do
    if Enum.all?(seq, &(&1 == 0)) do
      [seq]
    else
      new_seq =
        Enum.reduce(Enum.zip(seq, List.delete_at(seq, 0)), [], fn {prev, cur}, acc ->
          [cur - prev | acc]
        end)
        |> Enum.reverse()

      [seq | find_sequence(new_seq)]
    end
  end

  def next_in_sequence(seq) when is_list(seq) do
    Enum.reverse(seq)
    |> Enum.reduce(0, fn cur, acc ->
      List.last(cur) + acc
    end)
  end

  def previous_in_sequence(seq) when is_list(seq) do
    Enum.reverse(seq)
    |> Enum.reduce(0, fn cur, acc ->
      List.first(cur) - acc
    end)
  end

  def solve_pt1(input_data, _args) do
    parse(input_data) |> Enum.map(&find_sequence/1) |> Enum.map(&next_in_sequence/1) |> Enum.sum()
  end

  def solve_pt2(input_data, _args) do
    parse(input_data)
    |> Enum.map(&find_sequence/1)
    |> Enum.map(&previous_in_sequence/1)
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
