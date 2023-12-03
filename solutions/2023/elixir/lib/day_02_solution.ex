defmodule Day02 do
  @moduledoc """
  Advent Of Code 2023 - day 02
  https://adventofcode.com/2023/day/2

    --- Day 2: Cube Conundrum ---
  You're launched high into the atmosphere! The apex of your trajectory just barely reaches the
  surface of a large island floating in the sky. You gently land in a fluffy pile of leaves. It's
  quite cold, but you don't see much snow. An Elf runs over to greet you.

  The Elf explains that you've arrived at Snow Island and apologizes for the lack of snow. He'll be
  happy to explain the situation, but it's a bit of a walk, so you have some time. They don't get
  many visitors up here; would you like to play a game in the meantime?

  As you walk, the Elf shows you a small bag and some cubes which are either red, green, or blue.
  Each time you play this game, he will hide a secret number of cubes of each color in the bag, and
  your goal is to figure out information about the number of cubes.

  To get information, once a bag has been loaded with cubes, the Elf will reach into the bag, grab a
  handful of random cubes, show them to you, and then put them back in the bag. He'll do this a few
  times per game.

  You play several games and record the information from each game (your puzzle input). Each game is
  listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list of
  subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

  For example, the record of a few games might look like this:
  Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
  Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
  Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green


  In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set
  is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes;
  the third set is only 2 green cubes.

  The Elf would first like to know which games would have been possible if the bag contained only 12
  red cubes, 13 green cubes, and 14 blue cubes?

  In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with
  that configuration. However, game 3 would have been impossible because at one point the Elf showed
  you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed
  you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you
  get 8.

  Determine which games would have been possible if the bag had been loaded with only 12 red cubes,
  13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?

  --- Part Two ---
  The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure
  why the water stopped; however, he can show you how to get to the water source to check it out for
  yourself. It's just up ahead!

  As you continue your walk, the Elf poses a second question: in each game you played, what is the
  fewest number of cubes of each color that could have been in the bag to make the game possible?

  Again consider the example games from earlier:
  Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
  Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
  Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green


  In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any
  color had even one fewer cube, the game would have been impossible.
  Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
  Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
  Game 4 required at least 14 red, 3 green, and 15 blue cubes.
  Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.


  The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied
  together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560,
  630, and 36, respectively. Adding up these five powers produces the sum 2286.

  For each game, find the minimum set of cubes that must have been present. What is the sum of the
  power of these sets?
  """

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

  @doc """
      iex> Day02.solve_pt1(\"""
      ...>Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      ...>Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      ...>Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      ...>Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      ...>Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
      ...>\""")
      8
  """
  def solve_pt1(input) do
    maxCubes = [12, 13, 14]

    parse(input)
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

  @doc """
      iex> Day02.solve_pt2(\"""
      ...>Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      ...>Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      ...>Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      ...>Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      ...>Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
      ...>\""")
      2286
  """
  def solve_pt2(input) do
    parse(input)
    |> Enum.map(fn {game, cubes} ->
      Enum.reduce(cubes, [0, 0, 0], fn cube, acc ->
        Enum.zip(cube, acc)
        |> Enum.map(fn {prev, cur} -> if cur > prev, do: cur, else: prev end)
      end)
      |> Enum.reduce(1, fn color, acc -> color * acc end)
    end)
    |> Enum.sum()
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
