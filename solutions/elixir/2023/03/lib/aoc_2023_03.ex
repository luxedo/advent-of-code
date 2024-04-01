defmodule Year2023Day03 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 03
  Elixir Solution

  Day 3: Gear Ratios

  https://adventofcode.com/2023/day/3
  """
  import EsbFireplace

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Stream.with_index()
    |> Enum.flat_map(fn {line, idy} ->
      String.trim(line)
      |> String.graphemes()
      |> Stream.with_index()
      |> Enum.map(fn {char, idx} ->
        case char do
          "." ->
            %{:type => :empty, :c0 => {idy, idx}, :c1 => {idy, idx}, :value => char}

          n when n in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] ->
            %{:type => :number, :c0 => {idy, idx}, :c1 => {idy, idx}, :value => char}

          _ ->
            %{
              :type => :symbol,
              :c0 => {idy - 1, idx - 1},
              :c1 => {idy + 1, idx + 1},
              :value => char
            }
        end
      end)
      |> Enum.reduce([], fn cell, acc ->
        pcell = List.first(acc, %{})

        case {cell.type, Map.get(pcell, :type)} do
          {:number, :number} ->
            List.replace_at(
              acc,
              0,
              Map.merge(pcell, %{:c1 => cell.c1, :value => pcell.value <> cell.value})
            )

          _ ->
            [cell | acc]
        end
      end)
      |> Enum.reverse()
    end)
    |> Enum.map(fn cell ->
      case cell.type do
        :number -> Map.replace(cell, :value, String.to_integer(cell.value))
        _ -> cell
      end
    end)
    |> Enum.filter(fn %{:type => type} -> type != :empty end)
    |> Enum.split_with(fn %{:type => type} -> type == :number end)
  end

  def overlaps2d?(p0min, p0max, p1min, p1max) do
    {y0min, x0min} = p0min
    {y0max, x0max} = p0max
    {y1min, x1min} = p1min
    {y1max, x1max} = p1max
    overlaps1d?(y0min, y0max, y1min, y1max) && overlaps1d?(x0min, x0max, x1min, x1max)
  end

  def overlaps1d?(c0min, c0max, c1min, c1max) do
    c0max >= c1min && c1max >= c0min
  end

  def find_overlaps(%{:c0 => p0min, :c1 => p0max}, list1) do
    Enum.map(list1, fn cell1 = %{:c0 => p1min, :c1 => p1max} ->
      if overlaps2d?(p0min, p0max, p1min, p1max), do: cell1, else: nil
    end)
    |> Enum.filter(fn
      nil -> false
      _ -> true
    end)
  end

  def find_overlaps(list0, list1) when is_list(list0) and is_list(list1) do
    Enum.map(list0, fn cell0 -> {cell0, find_overlaps(cell0, list1)} end)
  end

  def solve_pt1(input_data, _args) do
    {digits, symbols} = parse(input_data)

    find_overlaps(digits, symbols)
    |> Enum.filter(fn {_, overlaps} -> length(overlaps) > 0 end)
    |> Enum.map(fn {cell, _} -> cell.value end)
    |> Enum.sum()
  end

  def solve_pt2(input_data, _args) do
    {digits, symbols} = parse(input_data)

    find_overlaps(Enum.filter(symbols, fn %{:value => value} -> value == "*" end), digits)
    |> Enum.filter(fn {_, overlaps} -> length(overlaps) == 2 end)
    |> Enum.map(fn {_, overlaps} ->
      Enum.map(overlaps, fn ov -> ov.value end) |> Enum.product()
    end)
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
