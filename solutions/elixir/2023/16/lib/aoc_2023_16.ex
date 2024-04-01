defmodule Year2023Day16 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 16
  Elixir Solution

  Day 16: The Floor Will Be Lava

  https://adventofcode.com/2023/day/16
  """
  import EsbFireplace

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

  defp solve_pt1(input_data, _args) do
    mirrors = parse(input_data)
    cursors = Cursor.move(mirrors, {0, -1, :e})
    count_energized(mirrors, cursors)
  end

  defp solve_pt2(input_data, _args) do
    mirrors = parse(input_data)
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

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
