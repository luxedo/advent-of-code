defmodule Year2023Day11 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 11
  Elixir Solution

  Day 11: Cosmic Expansion

  https://adventofcode.com/2023/day/11
  """
  import EsbFireplace

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      String.graphemes(line) |> Enum.map(&if &1 == ".", do: :s, else: :g)
    end)
  end

  def transpose(rows) do
    rows
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def expand_universe(universe, axis: 0) do
    width = Kernel.length(Enum.at(universe, 0))
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
        {_ = :g, x} -> {y, x}
        _ -> nil
      end)
    end)
    |> Enum.filter(&(&1 != nil))
  end

  def to_distance(_ = :e, expansion), do: expansion
  def to_distance(_, _), do: 1

  def space_distances(universe, expansion, axis: 1) do
    Enum.at(universe, 0) |> Enum.map(&to_distance(&1, expansion))
  end

  def space_distances(universe, expansion, axis: 0) do
    space_distances(transpose(universe), expansion, axis: 1)
  end

  def space_distances(universe, expansion) do
    {space_distances(universe, expansion, axis: 0), space_distances(universe, expansion, axis: 1)}
  end

  def galaxy_distances(pairs, {distances_y, distances_x}) do
    Enum.map(pairs, fn [{y0, x0}, {y1, x1}] ->
      [yl, yh] = Enum.sort([y0, y1])
      [xl, xh] = Enum.sort([x0, x1])

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
    galaxy_pairs = list_galaxies(universe) |> combinations(2)

    galaxy_distances(galaxy_pairs, {distances_y, distances_x})
    |> Enum.sum()
  end

  def solve_pt1(input_data, args) do
    expansion =
      case args do
        [value] -> String.to_integer(value)
        _ -> 2
      end

    solve(input_data, expansion)
  end

  def solve_pt2(input_data, args) do
    expansion =
      case args do
        [value] -> String.to_integer(value)
        _ -> 1_000_000
      end

    solve(input_data, expansion)
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
