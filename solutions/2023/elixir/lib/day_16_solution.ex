defmodule Day16 do
  @moduledoc """
  Advent Of Code 2023 - day 16
  https://adventofcode.com/2023/day/16

    --- Day 16: The Floor Will Be Lava ---
  With the beam of light completely focused somewhere, the reindeer leads you deeper still into the
  Lava Production Facility. At some point, you realize that the steel facility walls have been
  replaced with cave, and the doorways are just cave, and the floor is cave, and you're pretty sure
  this is actually just a giant cave.

  Finally, as you approach what must be the heart of the mountain, you see a bright light in a
  cavern up ahead. There, you discover that the beam of light you so carefully focused is emerging
  from the cavern wall closest to the facility and pouring all of its energy into a contraption on
  the opposite side.

  Upon closer inspection, the contraption appears to be a flat, two-dimensional square grid
  containing empty space (.), mirrors (/ and \), and splitters (| and -).

  The contraption is aligned so that most of the beam bounces around the grid, but each tile on the
  grid converts some of the beam's light into heat to melt the rock in the cavern.

  You note the layout of the contraption (your puzzle input). For example:
  .|...\....
  |.-.\.....
  .....|-...
  ........|.
  ..........
  .........\
  ..../.\\..
  .-.-/..|..
  .|....-|.\
  ..//.|....


  The beam enters in the top-left corner from the left and heading to the right. Then, its behavior
  depends on what it encounters as it moves:

  If the beam encounters empty space (.), it continues in the same direction.
  If the beam encounters a mirror (/ or \), the beam is reflected 90 degrees depending on the angle
  of the mirror. For instance, a rightward-moving beam that encounters a / mirror would continue
  upward in the mirror's column, while a rightward-moving beam that encounters a \ mirror would
  continue downward from the mirror's column.
  If the beam encounters the pointy end of a splitter (| or -), the beam passes through the splitter
  as if the splitter were empty space. For instance, a rightward-moving beam that encounters a -
  splitter would continue in the same direction.
  If the beam encounters the flat side of a splitter (| or -), the beam is split into two beams
  going in each of the two directions the splitter's pointy ends are pointing. For instance, a
  rightward-moving beam that encounters a | splitter would split into two beams: one that continues
  upward from the splitter's column and one that continues downward from the splitter's column.


  Beams do not interact with other beams; a tile can have many beams passing through it at the same
  time. A tile is energized if that tile has at least one beam pass through it, reflect in it, or
  split in it.

  In the above example, here is how the beam of light bounces around the contraption:
  >|<<<\....
  |v-.\^....
  .v...|->>>
  .v...v^.|.
  .v...v^...
  .v...v^..\
  .v../2\\..
  <->-/vv|..
  .|<<<2-|.\
  .v//.|.v..


  Beams are only shown on empty tiles; arrows indicate the direction of the beams. If a tile
  contains beams moving in multiple directions, the number of distinct directions is shown instead.
  Here is the same diagram but instead only showing whether a tile is energized (#) or not (.):
  ######....
  .#...#....
  .#...#####
  .#...##...
  .#...##...
  .#...##...
  .#..####..
  ########..
  .#######..
  .#...#.#..


  Ultimately, in this example, 46 tiles become energized.

  The light isn't energizing enough tiles to produce lava; to debug the contraption, you need to
  start by analyzing the current situation. With the beam starting in the top-left heading right,
  how many tiles end up being energized?

  --- Part Two ---
  As you try to work out what might be wrong, the reindeer tugs on your shirt and leads you to a
  nearby control panel. There, a collection of buttons lets you align the contraption so that the
  beam enters from any edge tile and heading away from that edge. (You can choose either of two
  directions for the beam if it starts on a corner; for instance, if the beam starts in the bottom-
  right corner, it can start heading either left or upward.)

  So, the beam could start on any tile in the top row (heading downward), any tile in the bottom row
  (heading upward), any tile in the leftmost column (heading right), or any tile in the rightmost
  column (heading left). To produce lava, you need to find the configuration that energizes as many
  tiles as possible.

  In the above example, this can be achieved by starting the beam in the fourth tile from the left
  in the top row:
  .|<2<\....
  |v-v\^....
  .v.v.|->>>
  .v.v.v^.|.
  .v.v.v^...
  .v.v.v^..\
  .v.v/2\\..
  <-2-/vv|..
  .|<<<2-|.\
  .v//.|.v..


  Using this configuration, 51 tiles are energized:
  .#####....
  .#.#.#....
  .#.#.#####
  .#.#.##...
  .#.#.##...
  .#.#.##...
  .#.#####..
  ########..
  .#######..
  .#...#.#..


  Find the initial beam configuration that energizes the largest number of tiles; how many tiles are
  energized in that configuration?
  """

  def parse(input) do
    String.split(input, "\n", trim: true) |> Enum.map(&String.graphemes/1)
  end

  def count_energized(_, energized, []) do
    Enum.map(energized, fn line ->
      Enum.filter(line, &(&1 !== %{n: 0, e: 0, s: 0, w: 0})) |> Enum.count()
    end)
    |> Enum.sum()
  end

  def count_energized(mirrors, energized, cursors) do
    {energized, cursors} =
      Enum.reduce(cursors, {energized, []}, fn cursor = {y, x, d}, {energized, cursors} ->
        if Cursor.is_energized(energized, {y, x, d}) do
          {energized, cursors}
        else
          energized = Cursor.energize(energized, {y, x, d})
          new_cursors = Cursor.move(mirrors, cursor)
          {energized, new_cursors ++ cursors}
        end
      end)

    count_energized(mirrors, energized, cursors)
  end

  def count_energized(mirrors, cursors) do
    {height, width} = {Kernel.length(mirrors), Enum.at(mirrors, 0) |> Kernel.length()}

    energized =
      for _ <- 1..height, do: for(_ <- 1..width, do: %{n: 0, e: 0, s: 0, w: 0})

    count_energized(mirrors, energized, cursors)
  end

  @doc """
      iex> Day16.solve_pt1(\~S\"""
      ...>.|...\\....
      ...>|.-.\\.....
      ...>.....|-...
      ...>........|.
      ...>..........
      ...>.........\\
      ...>..../.\\\\..
      ...>.-.-/..|..
      ...>.|....-|.\\
      ...>..//.|....
      ...>\""")
      46
  """
  def solve_pt1(input) do
    mirrors = parse(input)
    cursors = Cursor.move(mirrors, {0, -1, :e})
    count_energized(mirrors, cursors)
  end

  @doc """
      iex> Day16.solve_pt2(\~S\"""
      ...>.|...\\....
      ...>|.-.\\.....
      ...>.....|-...
      ...>........|.
      ...>..........
      ...>.........\\
      ...>..../.\\\\..
      ...>.-.-/..|..
      ...>.|....-|.\\
      ...>..//.|....
      ...>\""")
      51
  """
  def solve_pt2(input) do
    mirrors = parse(input)
    {height, width} = {Kernel.length(mirrors), Enum.at(mirrors, 0) |> Kernel.length()}

    (Enum.flat_map(
       0..width,
       fn x ->
         [
           count_energized(mirrors, Cursor.move(mirrors, {-1, x, :s})),
           count_energized(mirrors, Cursor.move(mirrors, {height, x, :n}))
         ]
       end
     ) ++
       Enum.flat_map(
         0..height,
         fn y ->
           [
             count_energized(mirrors, Cursor.move(mirrors, {y, -1, :e})),
             count_energized(mirrors, Cursor.move(mirrors, {y, width, :w}))
           ]
         end
       ))
    |> Enum.max()
  end

  def main do
    ans1 =
      File.read!("../data/day_16_input.txt")
      |> Day16.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_16_input.txt")
      |> Day16.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end

defmodule Cursor do
  def new(mirrors, cursor = {y, x, d}) do
    {height, width} = {Kernel.length(mirrors), Enum.at(mirrors, 0) |> Kernel.length()}

    if y < 0 || x < 0 || x >= width || y >= height do
      []
    else
      case {Enum.at(mirrors, y) |> Enum.at(x), d} do
        {".", _} -> [cursor]
        {"\\", :n} -> [{y, x, :w}]
        {"\\", :e} -> [{y, x, :s}]
        {"\\", :s} -> [{y, x, :e}]
        {"\\", :w} -> [{y, x, :n}]
        {"/", :n} -> [{y, x, :e}]
        {"/", :e} -> [{y, x, :n}]
        {"/", :s} -> [{y, x, :w}]
        {"/", :w} -> [{y, x, :s}]
        {"|", d} when d in [:e, :w] -> [{y, x, :n}, {y, x, :s}]
        {"-", d} when d in [:n, :s] -> [{y, x, :e}, {y, x, :w}]
        _ -> [cursor]
      end
    end
  end

  def move(mirrors, {y, x, d}) do
    case d do
      :n -> Cursor.new(mirrors, {y - 1, x, d})
      :e -> Cursor.new(mirrors, {y, x + 1, d})
      :s -> Cursor.new(mirrors, {y + 1, x, d})
      :w -> Cursor.new(mirrors, {y, x - 1, d})
    end
  end

  def energize(energized, {y, x, d}) do
    new = Enum.at(energized, y) |> Enum.at(x) |> Map.update!(d, &(&1 + 1))
    List.replace_at(energized, y, List.replace_at(Enum.at(energized, y), x, new))
  end

  def is_energized(energized, {y, x, d}) do
    Enum.at(energized, y) |> Enum.at(x) |> Map.fetch!(d) > 0
  end
end
