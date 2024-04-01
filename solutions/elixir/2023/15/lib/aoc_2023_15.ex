defmodule Year2023Day15 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 15
  Elixir Solution

  Day 15: Lens Library

  https://adventofcode.com/2023/day/15
  """
  import EsbFireplace

  def parse(input) do
    String.trim(input) |> String.split(",")
  end

  def hash(string) do
    String.graphemes(string) |> Enum.reduce(0, &hash/2)
  end

  def hash(char, acc) do
    # Determine the ASCII code for the current character of the string.
    c = :binary.first(char)
    # Increase the current value by the ASCII code you just determined.
    acc = acc + c
    # Set the current value to itself multiplied by 17.
    acc = acc * 17
    # Set the current value to the remainder of dividing itself by 256.
    Kernel.rem(acc, 256)
  end

  def put_in_box(boxes, box, lens = {code, _}) do
    Map.update(boxes, box, [lens], fn lenses ->
      case Enum.find_index(lenses, fn {c, _} -> c == code end) do
        nil -> [lens | lenses]
        i -> List.replace_at(lenses, i, lens)
      end
    end)
  end

  def remove_from_box(boxes, box, code) do
    Map.update(boxes, box, [], fn lenses ->
      case Enum.find_index(lenses, fn {c, _} -> c == code end) do
        nil -> lenses
        i -> List.delete_at(lenses, i)
      end
    end)
  end

  defp solve_pt1(input_data, _args) do
    parse(input_data)
    |> Enum.map(&hash/1)
    |> Enum.sum()
  end

  defp solve_pt2(input_data, _args) do
    parse(input_data)
    |> Enum.reduce(%{}, fn line, acc ->
      if String.contains?(line, "=") do
        [code, focal] = String.split(line, "=")
        box = hash(code)
        put_in_box(acc, box, {code, focal})
      else
        code = String.trim_trailing(line, "-")
        box = hash(code)
        remove_from_box(acc, box, code)
      end
    end)
    |> Enum.flat_map(fn {key, value} ->
      Enum.reverse(value)
      |> Enum.with_index()
      |> Enum.map(fn {{_, focal}, i} ->
        (key + 1) * (i + 1) * String.to_integer(focal)
      end)
    end)
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
