defmodule Day03 do
  @moduledoc """
  Advent Of Code 2023 - day 03
  https://adventofcode.com/2023/day/3

    --- Day 3: Gear Ratios ---
  You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up
  to the water source, but this is as far as he can bring you. You go inside.

  It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

  "Aaah!"

  You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I
  wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I
  can fix it." You offer to help.

  The engineer explains that an engine part seems to be missing from the engine, but nobody can
  figure out which one. If you can add up all the part numbers in the engine schematic, it should be
  easy to work out which part is missing.

  The engine schematic (your puzzle input) consists of a visual representation of the engine. There
  are lots of numbers and symbols you don't really understand, but apparently any number adjacent to
  a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do
  not count as a symbol.)

  Here is an example engine schematic:
  467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..


  In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114
  (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part
  number; their sum is 4361.

  Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers
  in the engine schematic?

  --- Part Two ---
  The engineer finds the missing part and installs it in the engine! As the engine springs to life,
  you jump in the closest gondola, finally ready to ascend to the water source.

  You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the
  gondola has a phone labeled "help", so you pick it up and the engineer answers.

  Before you can explain the situation, she suggests that you look out the window. There stands the
  engineer, holding a phone in one hand and waving with the other. You're going so slowly that you
  haven't even left the station. You exit the gondola.

  The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any *
  symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying
  those two numbers together.

  This time, you need to find the gear ratio of every gear and add them all up so that the engineer
  can figure out which gear needs to be replaced.

  Consider the same engine schematic again:
  467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..


  In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and
  35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490.
  (The * adjacent to 617 is not a gear because it is only adjacent to one part number.) Adding up
  all of the gear ratios produces 467835.

  What is the sum of all of the gear ratios in your engine schematic?
  """

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

  def find_overlaps(cell0 = %{:c0 => p0min, :c1 => p0max}, list1) do
    Enum.map(list1, fn cell1 = %{:c0 => p1min, :c1 => p1max} ->
      if overlaps2d?(p0min, p0max, p1min, p1max), do: cell1, else: nil
    end)
    |> Enum.filter(fn
      nil -> false
      _ -> true
    end)
  end

  @doc """
       iex> Day03.solve_pt1(\"""
       ...> 467..114..
       ...> ...*......
       ...> ..35..633.
       ...> ......#...
       ...> 617*......
       ...> .....+.58.
       ...> ..592.....
       ...> ......755.
       ...> ...$.*....
       ...> .664.598..
       ...>\""")
       4361
  """
  def solve_pt1(input) do
    {digits, symbols} = parse(input)

    Enum.filter(digits, fn cell0 -> length(find_overlaps(cell0, symbols)) > 0 end)
    |> Enum.map(fn %{:value => value} -> value end)
    |> Enum.sum()
  end

  @doc """
       iex> Day03.solve_pt2(\"""
       ...> 467..114..
       ...> ...*......
       ...> ..35..633.
       ...> ......#...
       ...> 617*......
       ...> .....+.58.
       ...> ..592.....
       ...> ......755.
       ...> ...$.*....
       ...> .664.598..
       ...>\""")
       467835
  """
  def solve_pt2(input) do
    {digits, symbols} = parse(input)

    Enum.map(symbols, fn cell0 ->
      find_overlaps(cell0, digits)
    end)
    |> Enum.filter(fn ovs -> length(ovs) == 2 end)
    |> Enum.map(fn ovs -> Enum.map(ovs, fn ov -> ov.value end) |> Enum.product() end)
    |> Enum.sum()
  end

  def main do
    ans1 =
      File.read!("../data/day_03_input.txt")
      |> Day03.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_03_input.txt")
      |> Day03.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
