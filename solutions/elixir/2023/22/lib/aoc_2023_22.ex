defmodule Year2023Day22 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 22
  Elixir Solution

  Day 22: Sand Slabs

  https://adventofcode.com/2023/day/22
  """
  import EsbFireplace

  defmodule Brick do
    defstruct [:idx, :xs, :ys, :zs]

    def from_list([[sx, sy, sz], [ex, ey, ez]], idx) do
      %Brick{idx: idx, xs: sx..ex, ys: sy..ey, zs: sz..ez}
    end

    def drop(b = %Brick{zs: z0..z1}) do
      Map.merge(b, %{zs: (z0 - 1)..(z1 - 1)})
    end
  end

  defmodule Pile do
    defstruct [
      :width,
      :depth,
      :height,
      :bricks,
      :bheight,
      :settled,
      :levels,
      :moves,
      :bricks_moved
    ]

    def parse(input) do
      bricks =
        String.split(input, "\n", trim: true)
        |> Enum.with_index()
        |> Enum.map(fn {line, idx} ->
          brick =
            String.split(line, "~")
            |> Enum.map(fn d -> String.split(d, ",") |> Enum.map(&String.to_integer/1) end)
            |> Brick.from_list(idx)

          {idx, brick}
        end)
        |> Map.new()

      Pile.new(bricks)
    end

    def new(bricks) do
      bheight =
        Enum.map(bricks, fn {k, %Brick{zs: z0.._}} ->
          {z0, k}
        end)
        |> Enum.reduce(%{}, fn {k, v}, acc ->
          Map.update(acc, k, [v], &[v | &1])
        end)

      {width, depth, height} =
        Enum.reduce(bricks, {0, 0, 0}, fn {_, %Brick{xs: x0..x1, ys: y0..y1, zs: z0..z1}},
                                          {mx, my, mz} ->
          {
            Enum.max([mx, x0, x1]),
            Enum.max([my, y0, y1]),
            Enum.max([mz, z0, z1])
          }
        end)

      levels =
        for(y <- 0..depth, do: for(x <- 0..width, do: {{y, x}, []}))
        |> List.flatten()
        |> Map.new()

      %Pile{
        width: width,
        depth: depth,
        height: height,
        bricks: bricks,
        bheight: bheight,
        settled: %{},
        levels: levels,
        moves: 0,
        bricks_moved: 0
      }
    end

    def drop(pile = %Pile{}) do
      case settle(pile) do
        {:halt, pile} -> pile
        {:cont, pile} -> drop(pile)
      end
    end

    def settle(pile = %Pile{bricks: bricks}) when map_size(bricks) == 0, do: {:halt, pile}

    def settle(
          pile = %Pile{
            bricks: bricks,
            bheight: bheight,
            settled: settled,
            levels: levels,
            moves: moves,
            bricks_moved: bricks_moved
          }
        ) do
      lowest = Map.keys(bheight) |> Enum.min()
      {bidxs, bheight} = Map.pop!(bheight, lowest)
      {st, bricks} = Map.split(bricks, bidxs)

      {levels, st, m, bm} =
        Enum.reduce(st, {levels, st, 0, 0}, fn {idx, brick}, {levels, st, m, bm} ->
          height = Pile.find_top(levels, brick)
          levels = Pile.place(brick, height, levels)
          z0..z1 = brick.zs
          dz = z1 - z0
          st = Map.replace(st, idx, Map.merge(brick, %{zs: height..(height + dz)}))
          bmi = if z0 == height, do: 0, else: 1
          {levels, st, m + z0 - height, bm + bmi}
        end)

      pile =
        Map.merge(pile, %{
          bricks: bricks,
          bheight: bheight,
          settled: Map.merge(settled, st),
          levels: levels,
          moves: moves + m,
          bricks_moved: bricks_moved + bm
        })

      {:cont, pile}
    end

    def find_top(levels, brick) do
      Enum.flat_map(brick.ys, fn y ->
        Enum.map(brick.xs, fn x ->
          case Map.fetch!(levels, {y, x}) do
            [] -> 0
            [{l, _} | _] -> l + 1
          end
        end)
      end)
      |> Enum.max()
    end

    def place(brick, height, levels) do
      new_levels =
        Enum.flat_map(brick.ys, fn y ->
          Enum.map(brick.xs, fn x ->
            z0..z1 = brick.zs
            {{y, x}, for(z <- z1..z0//-1, do: {z - z0 + height, brick.idx})}
          end)
        end)
        |> Map.new()

      Map.merge(levels, new_levels, fn _k, v1, v2 -> v2 ++ v1 end)
    end

    def remove(pile = %Pile{bricks: bricks, bheight: bheight}, idx) do
      {brick, bricks} = Map.pop!(bricks, idx)
      z0.._ = brick.zs
      bheight = Map.update!(bheight, z0, &List.delete(&1, idx))

      Map.merge(pile, %{
        bricks: bricks,
        bheight: bheight
      })
    end
  end

  defp solve_pt1(input_data, _args) do
    pile = Pile.parse(input_data) |> Pile.drop()
    fpile = Pile.new(pile.settled)

    Enum.filter(fpile.bricks, fn {idx, _} ->
      (Pile.remove(fpile, idx) |> Pile.drop()).moves == 0
    end)
    |> Enum.count()
  end

  defp solve_pt2(input_data, _args) do
    pile = Pile.parse(input_data) |> Pile.drop()
    fpile = Pile.new(pile.settled)

    Enum.map(fpile.bricks, fn {idx, _} ->
      (Pile.remove(fpile, idx) |> Pile.drop()).bricks_moved
    end) |> Enum.sum
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
