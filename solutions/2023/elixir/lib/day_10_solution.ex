defmodule Day10 do
  @moduledoc """
  Advent Of Code 2023 - day 10
  https://adventofcode.com/2023/day/10

    --- Day 10: Pipe Maze ---
  You use the hang glider to ride the hot air from Desert Island all the way up to the floating
  metal island. This island is surprisingly cold and there definitely aren't any thermals to glide
  on, so you leave your hang glider behind.

  You wander around for a while, but you don't find any people or animals. However, you do
  occasionally find signposts labeled "Hot Springs" pointing in a seemingly consistent direction;
  maybe you can find someone at the hot springs and ask them where the desert-machine parts are
  made.

  The landscape here is alien; even the flowers and trees are made of metal. As you stop to admire
  some metal grass, you notice something metallic scurry away in your peripheral vision and jump
  into a big pipe! It didn't look like any animal you've ever seen; if you want a better look,
  you'll need to get ahead of it.

  Scanning the area, you discover that the entire field you're standing on is densely packed with
  pipes; it was hard to tell at first because they're the same metallic silver color as the
  "ground". You make a quick sketch of all of the surface pipes you can see (your puzzle input).

  The pipes are arranged in a two-dimensional grid of tiles:

  | is a vertical pipe connecting north and south.
  - is a horizontal pipe connecting east and west.
  L is a 90-degree bend connecting north and east.
  J is a 90-degree bend connecting north and west.
  7 is a 90-degree bend connecting south and west.
  F is a 90-degree bend connecting south and east.
  . is ground; there is no pipe in this tile.
  S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't
  show what shape the pipe has.


  Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the
  animal is one large, continuous loop.

  For example, here is a square loop of pipe:
  .....
  .F-7.
  .|.|.
  .L-J.
  .....


  If the animal had entered this loop in the northwest corner, the sketch would instead look like
  this:
  .....
  .S-7.
  .|.|.
  .L-J.
  .....


  In the above diagram, the S tile is still a 90-degree F bend: you can tell because of how the
  adjacent pipes connect to it.

  Unfortunately, there are also many pipes that aren't connected to the loop! This sketch shows the
  same loop as above:
  -L|F7
  7S-7|
  L|7||
  -L-J|
  L|-JF


  In the above diagram, you can still figure out which pipes form the main loop: they're the ones
  connected to S, pipes those pipes connect to, pipes those pipes connect to, and so on. Every pipe
  in the main loop connects to its two neighbors (including S, which will have exactly two pipes
  connecting to it, and which is assumed to connect back to those two pipes).

  Here is a sketch that contains a slightly more complex main loop:
  ..F7.
  .FJ|.
  SJ.L7
  |F--J
  LJ...


  Here's the same example sketch with the extra, non-main-loop pipe tiles also shown:
  7-F7-
  .FJ|7
  SJLL7
  |F--J
  LJ.LJ


  If you want to get out ahead of the animal, you should find the tile in the loop that is farthest
  from the starting position. Because the animal is in the pipe, it doesn't make sense to measure
  this by direct distance. Instead, you need to find the tile that would take the longest number of
  steps along the loop to reach from the starting point - regardless of which way around the loop
  the animal went.

  In the first example with the square loop:
  .....
  .S-7.
  .|.|.
  .L-J.
  .....


  You can count the distance each tile in the loop is from the starting point like this:
  .....
  .012.
  .1.3.
  .234.
  .....


  In this example, the farthest point from the start is 4 steps away.

  Here's the more complex loop again:
  ..F7.
  .FJ|.
  SJ.L7
  |F--J
  LJ...


  Here are the distances for each tile on that loop:
  ..45.
  .236.
  01.78
  14567
  23...


  Find the single giant loop starting at S. How many steps along the loop does it take to get from
  the starting position to the point farthest from the starting position?

  --- Part Two ---
  You quickly reach the farthest point of the loop, but the animal never emerges. Maybe its nest is
  within the area enclosed by the loop?

  To determine whether it's even worth taking the time to search for such a nest, you should
  calculate how many tiles are contained within the loop. For example:
  ...........
  .S-------7.
  .|F-----7|.
  .||.....||.
  .||.....||.
  .|L-7.F-J|.
  .|..|.|..|.
  .L--J.L--J.
  ...........


  The above loop encloses merely four tiles - the two pairs of . in the southwest and southeast
  (marked I below). The middle . tiles (marked O below) are not in the loop. Here is the same loop
  again with those regions marked:
  ...........
  .S-------7.
  .|F-----7|.
  .||OOOOO||.
  .||OOOOO||.
  .|L-7OF-J|.
  .|II|O|II|.
  .L--JOL--J.
  .....O.....


  In fact, there doesn't even need to be a full tile path to the outside for tiles to count as
  outside the loop - squeezing between pipes is also allowed! Here, I is still within the loop and O
  is still outside the loop:
  ..........
  .S------7.
  .|F----7|.
  .||OOOO||.
  .||OOOO||.
  .|L-7F-J|.
  .|II||II|.
  .L--JL--J.
  ..........


  In both of the above examples, 4 tiles are enclosed by the loop.

  Here's a larger example:
  .F----7F7F7F7F-7....
  .|F--7||||||||FJ....
  .||.FJ||||||||L7....
  FJL7L7LJLJ||LJ.L-7..
  L--J.L7...LJS7F-7L7.
  ....F-J..F7FJ|L7L7L7
  ....L7.F7||L7|.L7L7|
  .....|FJLJ|FJ|F7|.LJ
  ....FJL-7.||.||||...
  ....L---J.LJ.LJLJ...


  The above sketch has many random bits of ground, some of which are in the loop (I) and some of
  which are outside it (O):
  OF----7F7F7F7F-7OOOO
  O|F--7||||||||FJOOOO
  O||OFJ||||||||L7OOOO
  FJL7L7LJLJ||LJIL-7OO
  L--JOL7IIILJS7F-7L7O
  OOOOF-JIIF7FJ|L7L7L7
  OOOOL7IF7||L7|IL7L7|
  OOOOO|FJLJ|FJ|F7|OLJ
  OOOOFJL-7O||O||||OOO
  OOOOL---JOLJOLJLJOOO


  In this larger example, 8 tiles are enclosed by the loop.

  Any tile that isn't part of the main loop can count as being enclosed by the loop. Here's another
  example with many bits of junk pipe lying around that aren't connected to the main loop at all:
  FF7FSF7F7F7F7F7F---7
  L|LJ||||||||||||F--J
  FL-7LJLJ||||||LJL-77
  F--JF--7||LJLJ7F7FJ-
  L---JF-JLJ.||-FJLJJ7
  |F|F-JF---7F7-L7L|7|
  |FFJF7L7F-JF7|JL---7
  7-L-JL7||F7|L7F-7F7|
  L.L7LFJ|||||FJL7||LJ
  L7JLJL-JLJLJL--JLJ.L


  Here are just the tiles that are enclosed by the loop marked with I:
  FF7FSF7F7F7F7F7F---7
  L|LJ||||||||||||F--J
  FL-7LJLJ||||||LJL-77
  F--JF--7||LJLJIF7FJ-
  L---JF-JLJIIIIFJLJJ7
  |F|F-JF---7IIIL7L|7|
  |FFJF7L7F-JF7IIL---7
  7-L-JL7||F7|L7F-7F7|
  L.L7LFJ|||||FJL7||LJ
  L7JLJL-JLJLJL--JLJ.L


  In this last example, 10 tiles are enclosed by the loop.

  Figure out whether you have time to search for the nest by calculating the area within the loop.
  How many tiles are enclosed by the loop?
  """
  require Integer

  @directions %{
    n: {-1, 0},
    e: {0, 1},
    s: {1, 0},
    w: {0, -1}
  }
  @opposite_directions %{
    n: {1, 0},
    e: {0, -1},
    s: {-1, 0},
    w: {0, 1}
  }

  @change_perspective %{
    n: :s,
    e: :w,
    s: :n,
    w: :e
  }

  @pipe_map %{
    "|" => %{
      value: "|",
      n: {:ok, :s},
      e: {:err},
      s: {:ok, :n},
      w: {:err}
    },
    "-" => %{
      value: "-",
      n: {:err},
      e: {:ok, :w},
      s: {:err},
      w: {:ok, :e}
    },
    "F" => %{
      value: "F",
      n: {:err},
      e: {:ok, :s},
      s: {:ok, :e},
      w: {:err}
    },
    "7" => %{
      value: "7",
      n: {:err},
      e: {:err},
      s: {:ok, :w},
      w: {:ok, :s}
    },
    "J" => %{
      value: "J",
      n: {:ok, :w},
      e: {:err},
      s: {:err},
      w: {:ok, :n}
    },
    "L" => %{
      value: "L",
      n: {:ok, :e},
      e: {:ok, :n},
      s: {:err},
      w: {:err}
    },
    "S" => %{
      value: "S",
      n: {:err},
      e: {:err},
      s: {:err},
      w: {:err}
    },
    "." => %{
      value: ".",
      n: {:err},
      e: {:err},
      s: {:err},
      w: {:err}
    }
  }

  def print_pipes(input) do
    Enum.map_join(input, "\n", fn line -> Enum.map_join(line, & &1.value) end) |> IO.puts()
  end

  def parse(input) do
    pipes =
      String.split(input, "\n", trim: true)
      |> Stream.with_index()
      |> Enum.map(fn {line, y} ->
        String.graphemes(line)
        |> Stream.with_index()
        |> Enum.map(fn {p, x} ->
          Map.fetch!(@pipe_map, p) |> Map.merge(%{y: y, x: x})
        end)
      end)

    start = List.flatten(pipes) |> Enum.find(&(&1.value == "S"))
    {pipes, start}
  end

  def iter_pipes(pipes, incoming, pipe) do
    {:ok, going} = pipe[incoming]
    {dy, dx} = @directions[going]
    next_pipe = Enum.at(pipes, pipe.y + dy) |> Enum.at(pipe.x + dx)
    {@change_perspective[going], next_pipe}
  end

  def find_cycle(pipes, start, incoming, pipe) do
    if start == pipe do
      [start]
    else
      {going, next_pipe} = iter_pipes(pipes, incoming, pipe)
      [pipe | find_cycle(pipes, start, going, next_pipe)]
    end
  end

  def find_cycle(pipes, start) do
    [{incoming, next_pipe}, _] =
      Enum.map(@opposite_directions, fn {d, {dy, dx}} ->
        {d, Enum.at(pipes, start.y + dy) |> Enum.at(start.x + dx)}
      end)
      |> Enum.filter(fn {d, p} -> Map.fetch!(p, d) != {:err} end)

    find_cycle(pipes, start, incoming, next_pipe)
  end

  def clamp_angle(angle) do
    cond do
      angle > :math.pi() -> clamp_angle(angle - 2 * :math.pi())
      angle < -:math.pi() -> clamp_angle(angle + 2 * :math.pi())
      true -> angle
    end
  end

  def winding_number({y, x}, polygon) do
    angles = Enum.map(polygon, fn {py, px} -> :math.atan2(y - py, x - px) end)

    ((Enum.zip(angles, [List.last(angles) | angles])
      |> Enum.map(fn {prev, cur} -> clamp_angle(cur - prev) end)
      |> Enum.sum()) / (2 * :math.pi()))
    |> Kernel.round()
  end

  @doc """
      iex> Day10.solve_pt1(\"""
      ...>-L|F7
      ...>7S-7|
      ...>L|7||
      ...>-L-J|
      ...>L|-JF
      ...>\""")
      4

      iex> Day10.solve_pt1(\"""
      ...>7-F7-
      ...>.FJ|7
      ...>SJLL7
      ...>|F--J
      ...>LJ.LJ
      ...>\""")
      8
  """
  def solve_pt1(input) do
    {pipes, start} = parse(input)
    ((find_cycle(pipes, start) |> Kernel.length()) + 1) |> Integer.floor_div(2)
  end

  @doc """
      iex> Day10.solve_pt2(\"""
      ...>......
      ...>.S-7..
      ...>.|.|..
      ...>.L-J..
      ...>......
      ...>......
      ...>\""")
      1

      iex> Day10.solve_pt2(\"""
      ...>...........
      ...>.S-------7.
      ...>.|F-----7|.
      ...>.||.....||.
      ...>.||.....||.
      ...>.|L-7.F-J|.
      ...>.|..|.|..|.
      ...>.L--J.L--J.
      ...>...........
      ...>\""")
      4

      iex> Day10.solve_pt2(\"""
      ...>.F----7F7F7F7F-7....
      ...>.|F--7||||||||FJ....
      ...>.||.FJ||||||||L7....
      ...>FJL7L7LJLJ||LJ.L-7..
      ...>L--J.L7...LJS7F-7L7.
      ...>....F-J..F7FJ|L7L7L7
      ...>....L7.F7||L7|.L7L7|
      ...>.....|FJLJ|FJ|F7|.LJ
      ...>....FJL-7.||.||||...
      ...>....L---J.LJ.LJLJ...
      ...>\""")
      8

      iex> Day10.solve_pt2(\"""
      ...>FF7FSF7F7F7F7F7F---7
      ...>L|LJ||||||||||||F--J
      ...>FL-7LJLJ||||||LJL-77
      ...>F--JF--7||LJLJ7F7FJ-
      ...>L---JF-JLJ.||-FJLJJ7
      ...>|F|F-JF---7F7-L7L|7|
      ...>|FFJF7L7F-JF7|JL---7
      ...>7-L-JL7||F7|L7F-7F7|
      ...>L.L7LFJ|||||FJL7||LJ
      ...>L7JLJL-JLJLJL--JLJ.L
      ...>\""")
      10
  """
  def solve_pt2(input) do
    {pipes, start} = parse(input)
    polygon = find_cycle(pipes, start) |> Enum.map(fn %{y: y, x: x} -> {y, x} end)

    {maxy, maxx} = {Kernel.length(pipes) - 1, Kernel.length(Enum.at(pipes, 0)) - 1}

    List.flatten(pipes)
    |> Enum.map(fn %{y: y, x: x} -> {y, x} end)
    |> Enum.filter(fn {y, x} -> x > 0 && x < maxx && y > 0 && y < maxy end)
    |> Enum.filter(&(!Enum.member?(polygon, &1)))
    |> Enum.filter(fn p -> winding_number(p, polygon) |> Integer.is_odd() end)
    |> Kernel.length()
  end

  def main do
    ans1 =
      File.read!("../data/day_10_input.txt")
      |> Day10.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_10_input.txt")
      |> Day10.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
