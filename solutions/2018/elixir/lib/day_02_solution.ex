defmodule Day02 do
  @moduledoc """
  Advent Of Code 2018 - day 02
  https://adventofcode.com/2018/day/2

   --- Day 2: Inventory Management System ---
  You stop falling through time, catch your breath, and check the screen on the device. "Destination
  reached. Current Year: 1518. Current Location: North Pole Utility Closet 83N10." You made it! Now,
  to find those anomalies.

  Outside the utility closet, you hear footsteps and a voice. "...I'm not sure either. But now that
  so many people have chimneys, maybe he could sneak in that way?" Another voice responds,
  "Actually, we've been working on a new kind of suit that would let him fit through tight spaces
  like that. But, I heard that a few days ago, they lost the prototype fabric, the design plans,
  everything! Nobody on the team can even seem to remember important details of the project!"

  "Wouldn't they have had enough fabric to fill several boxes in the warehouse? They'd be stored
  together, so the box IDs should be similar. Too bad it would take forever to search the warehouse
  for two similar box IDs..." They walk too far away to hear any more.

  Late at night, you sneak to the warehouse - who knows what kinds of paradoxes you could cause if
  you were discovered - and use your fancy wrist device to quickly scan every box and produce a list
  of the likely candidates (your puzzle input).

  To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number
  that have an ID containing exactly two of any letter and then separately counting those with
  exactly three of any letter. You can multiply those two counts together to get a rudimentary
  checksum and compare it to what your device predicts.

  For example, if you see the following box IDs:

  abcdef contains no letters that appear exactly two or three times.
  bababc contains two a and three b, so it counts for both.
  abbcde contains two b, but no letter appears exactly three times.
  abcccd contains three c, but no letter appears exactly two times.
  aabcdd contains two a and two d, but it only counts once.
  abcdee contains two e.
  ababab contains three a and three b, but it only counts once.


  Of these box IDs, four of them contain a letter which appears exactly twice, and three of them
  contain a letter which appears exactly three times. Multiplying these together produces a checksum
  of 4 * 3 = 12.

  What is the checksum for your list of box IDs?

  --- Part Two ---
  Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype
  fabric.

  The boxes will have IDs which differ by exactly one character at the same position in both
  strings. For example, given the following box IDs:
  abcde
  fghij
  klmno
  pqrst
  fguij
  axcye
  wvxyz


  The IDs abcde and axcye are close, but they differ by two characters (the second and fourth).
  However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must
  be the correct boxes.

  What letters are common between the two correct box IDs? (In the example above, this is found by
  removing the differing character from either ID, producing fgij.)
  """

  def parse(input) do
    input
    |> String.trim()
    |> String.split("\n", trim: true)
  end

  def count_letters(input) do
    input
    |> String.graphemes()
    |> Enum.reduce(%{}, fn cur, acc ->
      acc |> Map.update(cur, 1, fn c -> c + 1 end)
    end)
  end

  def invert_map(map) do
    map
    |> Enum.reduce(%{}, fn {_, v}, acc ->
      acc |> Map.update(v, 1, fn c -> c + 1 end)
    end)
  end

  @doc """
       iex> Day02.solve_pt1(\"""
       ...> abcdef
       ...> bababc
       ...> abbcde
       ...> abcccd
       ...> aabcdd
       ...> abcdee
       ...> ababab
       ...> \""")
       12
  """
  def solve_pt1(input) do
    dup_ids =
      input
      |> parse
      |> Enum.map(fn line -> line |> count_letters |> invert_map end)
      |> Enum.reduce(%{}, fn cur, acc ->
        cur
        |> Map.keys()
        |> Enum.reduce(acc, fn k, acc -> acc |> Map.update(k, 1, fn c -> c + 1 end) end)
      end)

    Map.get(dup_ids, 2, 0) * Map.get(dup_ids, 3, 0)
  end

  @doc """
       iex> Day02.solve_pt2(\"""
       ...> abcde
       ...> fghij
       ...> klmno
       ...> pqrst
       ...> fguij
       ...> axcye
       ...> wvxyz
       ...> \""")
       "fgij"
  """
  def solve_pt2(input) do
    ids = input |> parse

    {ix1, {ix2, _}} =
      Enum.slice(ids, 0..-2)
      |> Stream.with_index()
      |> Enum.map(fn {item, idx1} ->
        {idx1,
         Enum.slice(ids, (idx1 + 1)..-1)
         |> Stream.with_index()
         |> Enum.map(fn {other, idx2} ->
           {idx2, compare_strings(item, other)}
         end)
         |> Enum.max_by(fn {_, x} -> x end)}
      end)
      |> Enum.max_by(fn {_, {_, x}} -> x end)

    subtract_strings(Enum.at(ids, ix1), Enum.at(ids, ix1 + ix2 + 1))
  end

  def compare_strings(str1, str2) do
    Enum.zip([String.graphemes(str1), String.graphemes(str2)])
    |> Enum.map(fn {a, b} -> (a == b && 1) || 0 end)
    |> Enum.sum()
  end

  def subtract_strings(str1, str2) do
    Enum.zip([String.graphemes(str1), String.graphemes(str2)])
    |> Enum.filter(fn {a, b} -> a == b end) |> Enum.map_join(&(&1 |> Kernel.elem(0)))
  end

  def main do
    ans1 =
      File.read!("../data/day_02_input.txt")
      |> Day02.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_02_input.txt")
      |> Day02.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
