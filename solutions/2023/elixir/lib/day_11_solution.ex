defmodule Day11 do
  @moduledoc """
  Advent Of Code 2023 - day 11
  https://adventofcode.com/2023/day/11

    --- Day 11: Cosmic Expansion ---
  You continue following signs for "Hot Springs" and eventually come across an observatory. The Elf
  within turns out to be a researcher studying cosmic expansion using the giant telescope here.

  He doesn't know anything about the missing machine parts; he's only visiting for this research
  project. However, he confirms that the hot springs are the next-closest area likely to have
  people; he'll even take you straight there once he's done with today's observation analysis.

  Maybe you can help him with the analysis to speed things up?

  The researcher has collected a bunch of data and compiled the data into a single giant image (your
  puzzle input). The image includes empty space (.) and galaxies (#). For example:
  ...#......
  .......#..
  #.........
  ..........
  ......#...
  .#........
  .........#
  ..........
  .......#..
  #...#.....


  The researcher is trying to figure out the sum of the lengths of the shortest path between every
  pair of galaxies. However, there's a catch: the universe expanded in the time it took the light
  from those galaxies to reach the observatory.

  Due to something involving gravitational effects, only some space expands. In fact, the result is
  that any rows or columns that contain no galaxies should all actually be twice as big.

  In the above example, three columns and two rows contain no galaxies:
     v  v  v
   ...#......
   .......#..
   #.........
  >..........<
   ......#...
   .#........
   .........#
  >..........<
   .......#..
   #...#.....
     ^  ^  ^


  These rows and columns need to be twice as big; the result of cosmic expansion therefore looks
  like this:
  ....#........
  .........#...
  #............
  .............
  .............
  ........#....
  .#...........
  ............#
  .............
  .............
  .........#...
  #....#.......


  Equipped with this expanded universe, the shortest path between every pair of galaxies can be
  found. It can help to assign every galaxy a unique number:
  ....1........
  .........2...
  3............
  .............
  .............
  ........4....
  .5...........
  ............6
  .............
  .............
  .........7...
  8....9.......


  In these 9 galaxies, there are 36 pairs. Only count each pair once; order within the pair doesn't
  matter. For each pair, find any shortest path between the two galaxies using only steps that move
  up, down, left, or right exactly one . or # at a time. (The shortest path between two galaxies is
  allowed to pass through another galaxy.)

  For example, here is one of the shortest paths between galaxies 5 and 9:
  ....1........
  .........2...
  3............
  .............
  .............
  ........4....
  .5...........
  .##.........6
  ..##.........
  ...##........
  ....##...7...
  8....9.......


  This path has length 9 because it takes a minimum of nine steps to get from galaxy 5 to galaxy 9
  (the eight locations marked # plus the step onto galaxy 9 itself). Here are some other example
  shortest path lengths:

  Between galaxy 1 and galaxy 7: 15
  Between galaxy 3 and galaxy 6: 17
  Between galaxy 8 and galaxy 9: 5


  In this example, after expanding the universe, the sum of the shortest path between all 36 pairs
  of galaxies is 374.

  Expand the universe, then find the length of the shortest path between every pair of galaxies.
  What is the sum of these lengths?

  --- Part Two ---
  The galaxies are much older (and thus much farther apart) than the researcher initially estimated.

  Now, instead of the expansion you did before, make each empty row or column one million times
  larger. That is, each empty row should be replaced with 1000000 empty rows, and each empty column
  should be replaced with 1000000 empty columns.

  (In the example above, if each empty row or column were merely 10 times larger, the sum of the
  shortest paths between every pair of galaxies would be 1030. If each empty row or column were
  merely 100 times larger, the sum of the shortest paths between every pair of galaxies would be
  8410. However, your universe will need to expand far beyond these values.)

  Starting with the same initial image, expand the universe according to these new rules, then find
  the length of the shortest path between every pair of galaxies. What is the sum of these lengths?
  """

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line -> String.graphemes(line) |> Enum.map(&if &1 == ".", do: :s, else: :g) end)
  end

  def transpose(rows) do
    rows
    |> List.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def expand_universe(universe, axis: 0) do
    width = Kernel.length(Enum.at(universe, 0))
    blank_space = for _ <- 1..width, do: :s
    expanded_space = for _ <- 1..width, do: :e

    Enum.map(universe, fn
      row -> if Enum.member?(row, :g), do: row, else: expanded_space
    end)
  end

  def expand_universe(universe, axis: 1) do
    transpose(universe) |> expand_universe(axis: 0) |> transpose()
  end

  def expand_universe(universe) do
    expand_universe(universe, axis: 0) |> expand_universe(axis: 1)
  end

  def list_galaxies(universe) do
    Enum.with_index(universe)
    |> Enum.flat_map(fn {row, y} ->
      Enum.with_index(row)
      |> Enum.map(fn
        {s = :g, x} -> {y, x}
        _ -> nil
      end)
    end)
    |> Enum.filter(&(&1 != nil))
  end

  def to_distance(_ = :e, expansion), do: expansion
  def to_distance(s, _), do: 1

  def space_distances(universe, expansion, axis: 1) do
    Enum.at(universe, 0) |> Enum.map(&to_distance(&1, expansion))
  end

  def space_distances(universe, expansion, axis: 0) do
    space_distances(transpose(universe), expansion, axis: 1)
  end

  def space_distances(universe, expansion) do
    {space_distances(universe, expansion, axis: 0),
     space_distances(universe, expansion, axis: 1)}
  end

  def galaxy_distances(pairs, {distances_y, distances_x}) do
    Enum.map(pairs, fn [{y0, x0}, {y1, x1}] ->
      [yl, yh] = Enum.sort([y0, y1])
      [xl, xh] = Enum.sort([x0, x1])

      d =
        (Enum.slice(distances_y, yl..yh)
         |> Enum.sum()) +
          (Enum.slice(distances_x, xl..xh)
           |> Enum.sum()) - 2

      # [{y0, x0}, {y1, x1}, d, Enum.slice(distances_x, xl..xh), Enum.slice(distances_y, yl..yh)] |> IO.inspect
    end)
  end

  def combinations(_, 0), do: [[]]
  def combinations([], _), do: []

  def combinations([head | tail], size) do
    for(elem <- combinations(tail, size - 1), do: [head | elem]) ++ combinations(tail, size)
  end

  def solve(input, expansion) do
    universe = parse(input) |> expand_universe
    {distances_y, distances_x} = space_distances(universe, expansion)
    {length(distances_x), length(distances_y)}
    galaxy_pairs = list_galaxies(universe) |> combinations(2)

    galaxy_distances(galaxy_pairs, {distances_y, distances_x})
    |> Enum.sum()
  end

  @doc """
      iex> Day11.solve_pt1(\"""
      ...>...#......
      ...>.......#..
      ...>#.........
      ...>..........
      ...>......#...
      ...>.#........
      ...>.........#
      ...>..........
      ...>.......#..
      ...>#...#.....
      ...>\""")
      374
  """
  def solve_pt1(input) do
    solve(input, 2)
  end

  @doc """
      iex> Day11.solve(\"""
      ...>...#......
      ...>.......#..
      ...>#.........
      ...>..........
      ...>......#...
      ...>.#........
      ...>.........#
      ...>..........
      ...>.......#..
      ...>#...#.....
      ...>\""", 10)
      1030

      iex> Day11.solve(\"""
      ...>...#......
      ...>.......#..
      ...>#.........
      ...>..........
      ...>......#...
      ...>.#........
      ...>.........#
      ...>..........
      ...>.......#..
      ...>#...#.....
      ...>\""", 100)
      8410

      iex> Day11.solve_pt2(\"""
      ...>...#......
      ...>.......#..
      ...>#.........
      ...>..........
      ...>......#...
      ...>.#........
      ...>.........#
      ...>..........
      ...>.......#..
      ...>#...#.....
      ...>\""")
      82000210

  """
  def solve_pt2(input) do
    solve(input, 1000000)
  end

  def main do
    ans1 =
      File.read!("../data/day_11_input.txt")
      |> Day11.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_11_input.txt")
      |> Day11.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
