defmodule Day18 do
  @moduledoc """
  Advent Of Code 2023 - day 18
  https://adventofcode.com/2023/day/18

    --- Day 18: Lavaduct Lagoon ---
  Thanks to your efforts, the machine parts factory is one of the first factories up and running
  since the lavafall came back. However, to catch up with the large backlog of parts requests, the
  factory will also need a large supply of lava for a while; the Elves have already started creating
  a large lagoon nearby for this purpose.

  However, they aren't sure the lagoon will be big enough; they've asked you to take a look at the
  dig plan (your puzzle input). For example:
  R 6 (#70c710)
  D 5 (#0dc571)
  L 2 (#5713f0)
  D 2 (#d2c081)
  R 2 (#59c680)
  D 2 (#411b91)
  L 5 (#8ceee2)
  U 2 (#caa173)
  L 1 (#1b58a2)
  U 2 (#caa171)
  R 2 (#7807d2)
  U 3 (#a77fa3)
  L 2 (#015232)
  U 2 (#7a21e3)


  The digger starts in a 1 meter cube hole in the ground. They then dig the specified number of
  meters up (U), down (D), left (L), or right (R), clearing full 1 meter cubes as they go. The
  directions are given as seen from above, so if "up" were north, then "right" would be east, and so
  on. Each trench is also listed with the color that the edge of the trench should be painted as an
  RGB hexadecimal color code.

  When viewed from above, the above example dig plan would result in the following loop of trench
  (#) having been dug out from otherwise ground-level terrain (.):
  #######
  #.....#
  ###...#
  ..#...#
  ..#...#
  ###.###
  #...#..
  ##..###
  .#....#
  .######


  At this point, the trench could contain 38 cubic meters of lava. However, this is just the edge of
  the lagoon; the next step is to dig out the interior so that it is one meter deep as well:
  #######
  #######
  #######
  ..#####
  ..#####
  #######
  #####..
  #######
  .######
  .######


  Now, the lagoon can contain a much more respectable 62 cubic meters of lava. While the interior is
  dug out, the edges are also painted according to the color codes in the dig plan.

  The Elves are concerned the lagoon won't be large enough; if they follow their dig plan, how many
  cubic meters of lava could it hold?

  --- Part Two ---
  The Elves were right to be concerned; the planned lagoon would be much too small.

  After a few minutes, someone realizes what happened; someone swapped the color and instruction
  parameters when producing the dig plan. They don't have time to fix the bug; one of them asks if
  you can extract the correct instructions from the hexadecimal codes.

  Each hexadecimal code is six hexadecimal digits long. The first five hexadecimal digits encode the
  distance in meters as a five-digit hexadecimal number. The last hexadecimal digit encodes the
  direction to dig: 0 means R, 1 means D, 2 means L, and 3 means U.

  So, in the above example, the hexadecimal codes can be converted into the true instructions:

  #70c710 = R 461937
  #0dc571 = D 56407
  #5713f0 = R 356671
  #d2c081 = D 863240
  #59c680 = R 367720
  #411b91 = D 266681
  #8ceee2 = L 577262
  #caa173 = U 829975
  #1b58a2 = L 112010
  #caa171 = D 829975
  #7807d2 = L 491645
  #a77fa3 = U 686074
  #015232 = L 5411
  #7a21e3 = U 500254


  Digging out this loop and its interior produces a lagoon that can hold an impressive 952408144115
  cubic meters of lava.

  Convert the hexadecimal color codes into the correct instructions; if the Elves follow this new
  dig plan, how many cubic meters of lava could the lagoon hold?
  """
  require Integer

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      [side, steps, color] = String.split(line, " ")
      steps = String.to_integer(steps)

      color = String.trim_leading(color, "(#") |> String.trim_trailing(")")

      side2 =
        case String.last(color) do
          "0" -> "R"
          "1" -> "D"
          "2" -> "L"
          "3" -> "U"
        end

      {steps2, ""} = String.slice(color, 0..4) |> Integer.parse(16)

      {side, steps, {side2, steps2}}
    end)
  end

  defmodule Digger do
    defstruct pos: {0, 0}

    def move(%Digger{pos: {y, x}}, side, steps) do
      {dy, dx} =
        case side do
          "R" -> {0, steps}
          "D" -> {steps, 0}
          "L" -> {0, -steps}
          "U" -> {-steps, 0}
        end

      %Digger{pos: {y + dy, x + dx}}
    end
  end

  defmodule Trench do
    defstruct [:sparse, height: nil, width: nil]

    def dig_plans(plans) do
      {_, history} =
        Enum.reduce(plans, {%Digger{}, []}, fn {side, steps}, {digger, history} ->
          digger = Digger.move(digger, side, steps)
          {digger, [digger.pos | history]}
        end)

      center(history)
    end

    def center(sparse) do
      miny = Enum.map(sparse, &elem(&1, 0)) |> Enum.min()
      maxy = Enum.map(sparse, &elem(&1, 0)) |> Enum.max()
      minx = Enum.map(sparse, &elem(&1, 1)) |> Enum.min()
      maxx = Enum.map(sparse, &elem(&1, 1)) |> Enum.max()

      sparse = Enum.map(sparse, fn {y, x} -> {y - miny, x - minx} end)

      %Trench{sparse: sparse, height: maxy - miny, width: maxx - minx}
    end

    def area(%Trench{sparse: sparse}) do
      {area2, perimeter, _} =
        Enum.reduce(sparse, {0, 0, List.last(sparse)}, fn {y, x}, {area, perimeter, {py, px}} ->
          area = area + (py + y) * (px - x)
          perimeter = perimeter + abs(x - px) + abs(y - py)
          {area, perimeter, {y, x}}
        end)

      trunc(Kernel.abs(area2 / 2) + perimeter / 2 + 1)
    end
  end

  @doc """
      iex> Day18.solve_pt1(\"""
      ...>R 6 (#70c710)
      ...>D 5 (#0dc571)
      ...>L 2 (#5713f0)
      ...>D 2 (#d2c081)
      ...>R 2 (#59c680)
      ...>D 2 (#411b91)
      ...>L 5 (#8ceee2)
      ...>U 2 (#caa173)
      ...>L 1 (#1b58a2)
      ...>U 2 (#caa171)
      ...>R 2 (#7807d2)
      ...>U 3 (#a77fa3)
      ...>L 2 (#015232)
      ...>U 2 (#7a21e3)
      ...>\""")
      62
  """
  def solve_pt1(input) do
    plans = parse(input) |> Enum.map(fn {d, s, _} -> {d, s} end)
    Trench.dig_plans(plans) |> Trench.area()
  end

  @doc """
      iex> Day18.solve_pt2(\"""
      ...>R 6 (#70c710)
      ...>D 5 (#0dc571)
      ...>L 2 (#5713f0)
      ...>D 2 (#d2c081)
      ...>R 2 (#59c680)
      ...>D 2 (#411b91)
      ...>L 5 (#8ceee2)
      ...>U 2 (#caa173)
      ...>L 1 (#1b58a2)
      ...>U 2 (#caa171)
      ...>R 2 (#7807d2)
      ...>U 3 (#a77fa3)
      ...>L 2 (#015232)
      ...>U 2 (#7a21e3)
      ...>\""")
      952408144115
  """
  def solve_pt2(input) do
    plans = parse(input) |> Enum.map(fn {_, _, p} -> p end)
    Trench.dig_plans(plans) |> Trench.area()
  end

  def main do
    ans1 =
      File.read!("../data/day_18_input.txt")
      |> Day18.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_18_input.txt")
      |> Day18.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
