defmodule Day17 do
  @moduledoc """
  Advent Of Code 2023 - day 17
  https://adventofcode.com/2023/day/17

    --- Day 17: Clumsy Crucible ---
  The lava starts flowing rapidly once the Lava Production Facility is operational. As you leave,
  the reindeer offers you a parachute, allowing you to quickly reach Gear Island.

  As you descend, your bird's-eye view of Gear Island reveals why you had trouble finding anyone on
  your way up: half of Gear Island is empty, but the half below you is a giant factory city!

  You land near the gradually-filling pool of lava at the base of your new lavafall. Lavaducts will
  eventually carry the lava throughout the city, but to make use of it immediately, Elves are
  loading it into large crucibles on wheels.

  The crucibles are top-heavy and pushed by hand. Unfortunately, the crucibles become very difficult
  to steer at high speeds, and so it can be hard to go in a straight line for very long.

  To get Desert Island the machine parts it needs as soon as possible, you'll need to find the best
  way to get the crucible from the lava pool to the machine parts factory. To do this, you need to
  minimize heat loss while choosing a route that doesn't require the crucible to go in a straight
  line for too long.

  Fortunately, the Elves here have a map (your puzzle input) that uses traffic patterns, ambient
  temperature, and hundreds of other parameters to calculate exactly how much heat loss can be
  expected for a crucible entering any particular city block.

  For example:
  2413432311323
  3215453535623
  3255245654254
  3446585845452
  4546657867536
  1438598798454
  4457876987766
  3637877979653
  4654967986887
  4564679986453
  1224686865563
  2546548887735
  4322674655533


  Each city block is marked by a single digit that represents the amount of heat loss if the
  crucible enters that block. The starting point, the lava pool, is the top-left city block; the
  destination, the machine parts factory, is the bottom-right city block. (Because you already start
  in the top-left block, you don't incur that block's heat loss unless you leave that block and then
  return to it.)

  Because it is difficult to keep the top-heavy crucible going in a straight line for very long, it
  can move at most three blocks in a single direction before it must turn 90 degrees left or right.
  The crucible also can't reverse direction; after entering each city block, it may only turn left,
  continue straight, or turn right.

  One way to minimize heat loss is this path:
  2>>34^>>>1323
  32v>>>35v5623
  32552456v>>54
  3446585845v52
  4546657867v>6
  14385987984v4
  44578769877v6
  36378779796v>
  465496798688v
  456467998645v
  12246868655<v
  25465488877v5
  43226746555v>


  This path never moves more than three consecutive blocks in the same direction and incurs a heat
  loss of only 102.

  Directing the crucible from the lava pool to the machine parts factory, but not moving more than
  three consecutive blocks in the same direction, what is the least heat loss it can incur?

  --- Part Two ---
  The crucibles of lava simply aren't large enough to provide an adequate supply of lava to the
  machine parts factory. Instead, the Elves are going to upgrade to ultra crucibles.

  Ultra crucibles are even more difficult to steer than normal crucibles. Not only do they have
  trouble going in a straight line, but they also have trouble turning!

  Once an ultra crucible starts moving in a direction, it needs to move a minimum of four blocks in
  that direction before it can turn (or even before it can stop at the end). However, it will
  eventually start to get wobbly: an ultra crucible can move a maximum of ten consecutive blocks
  without turning.

  In the above example, an ultra crucible could follow this path to minimize heat loss:
  2>>>>>>>>1323
  32154535v5623
  32552456v4254
  34465858v5452
  45466578v>>>>
  143859879845v
  445787698776v
  363787797965v
  465496798688v
  456467998645v
  122468686556v
  254654888773v
  432267465553v


  In the above example, an ultra crucible would incur the minimum possible heat loss of 94.

  Here's another example:
  111111111111
  999999999991
  999999999991
  999999999991
  999999999991


  Sadly, an ultra crucible would need to take an unfortunate path like this one:
  1>>>>>>>1111
  9999999v9991
  9999999v9991
  9999999v9991
  9999999v>>>>


  This route causes the ultra crucible to incur the minimum possible heat loss of 71.

  Directing the ultra crucible from the lava pool to the machine parts factory, what is the least
  heat loss it can incur?
  """

  defmodule Heap do
    defstruct data: nil, size: 0
    def new(), do: %Heap{}
    def new(values) when is_list(values), do: Enum.reduce(values, %Heap{}, &Heap.push(&2, &1))
    def new(value), do: %Heap{} |> push(value)

    def push(%Heap{data: h, size: n}, value) do
      %Heap{data: meld(h, {value, []}), size: n + 1}
    end

    def pop(%Heap{data: nil, size: 0}), do: nil
    def pop(%Heap{data: {_, q}, size: n}), do: %Heap{data: pair(q), size: n - 1}

    def root(%Heap{data: {v, _}}), do: v
    def root(%Heap{data: nil, size: 0}), do: nil

    def size(%Heap{size: n}), do: n

    def split(%Heap{} = heap), do: {Heap.root(heap), Heap.pop(heap)}

    defp meld(nil, queue), do: queue
    defp meld(queue, nil), do: queue

    defp meld({k0 = {rank0, _}, l0}, {{rank1, _}, _} = r) when rank0 < rank1,
      do: {k0, [r | l0]}

    defp meld({_, _} = l, {k1, r0}), do: {k1, [l | r0]}

    defp pair([]), do: nil
    defp pair([q]), do: q

    defp pair([q0, q1 | q]) do
      meld(q0, q1) |> meld(pair(q))
    end
  end

  defmodule City do
    defstruct blocks: nil, height: 0, width: 0

    def new(input) do
      blocks =
        String.split(input, "\n", trim: true)
        |> Enum.map(fn line ->
          String.graphemes(line)
          |> Enum.map(&String.to_integer/1)
        end)

      {height, width} = {Kernel.length(blocks), Enum.at(blocks, 0) |> Kernel.length()}
      %City{blocks: blocks, height: height, width: width}
    end

    def get(%City{blocks: blocks}, {y, x}) do
      Enum.at(blocks, y) |> Enum.at(x)
    end
  end

  def dijkstra(
        city = %City{height: height, width: width},
        heap,
        visited,
        finish,
        crucible = {lmin, lmax}
      ) do
    {{r, node = {current = {y, x}, direction = {dy, dx}, l}}, heap} = Heap.split(heap)

    cond do
      node in visited ->
        dijkstra(city, heap, visited, finish, crucible)

      current == finish ->
        r

      true ->
        visited = MapSet.put(visited, node)

        possible =
          List.flatten([
            if(l < lmax, do: [{{y + dy, x + dx}, direction, l + 1}], else: []),
            if(l >= lmin,
              do: [
                {{y + dx, x + dy}, {dx, dy}, 1},
                {{y - dx, x - dy}, {-dx, -dy}, 1}
              ],
              else: []
            )
          ])
          |> Enum.filter(fn next = {{y, x}, _, _} ->
            y in 0..(height - 1) && x in 0..(width - 1) && next not in visited
          end)

        heap =
          Enum.reduce(possible, heap, fn next = {c, _, _}, h ->
            Heap.push(h, {r + City.get(city, c), next})
          end)

        dijkstra(city, heap, visited, finish, crucible)
    end
  end

  def find_least_heat_path(
        city = %City{height: height, width: width},
        {sy, sx},
        finish,
        crucible
      ) do
    heap =
      Enum.filter([{0, 1}, {1, 0}, {0, -1}, {-1, 0}], fn {y, x} ->
        (sy + y) in 0..(height - 1) && (sx + x) in 0..(width - 1)
      end)
      |> Enum.map(fn dir = {dy, dx} ->
        c = {sy + dy, sx + dx}
        {City.get(city, c), {c, dir, 1}}
      end)
      |> Heap.new()

    visited = MapSet.new()

    dijkstra(city, heap, visited, finish, crucible)
  end

  @doc """
      iex> Day17.solve_pt1(\"""
      ...>2413432311323
      ...>3215453535623
      ...>3255245654254
      ...>3446585845452
      ...>4546657867536
      ...>1438598798454
      ...>4457876987766
      ...>3637877979653
      ...>4654967986887
      ...>4564679986453
      ...>1224686865563
      ...>2546548887735
      ...>4322674655533
      ...>\""")
      102
  """
  def solve_pt1(input) do
    city = City.new(input)
    {start, finish} = {{0, 0}, {city.height - 1, city.width - 1}}
    crucible = {1, 3}
    find_least_heat_path(city, start, finish, crucible)
  end

  @doc """
      iex> Day17.solve_pt2(\"""
      ...>2413432311323
      ...>3215453535623
      ...>3255245654254
      ...>3446585845452
      ...>4546657867536
      ...>1438598798454
      ...>4457876987766
      ...>3637877979653
      ...>4654967986887
      ...>4564679986453
      ...>1224686865563
      ...>2546548887735
      ...>4322674655533
      ...>\""")
      94
  """
  def solve_pt2(input) do
    city = City.new(input)
    {start, finish} = {{0, 0}, {city.height - 1, city.width - 1}}
    crucible = {4, 10}
    find_least_heat_path(city, start, finish, crucible)
  end

  def main do
    ans1 =
      File.read!("../data/day_17_input.txt")
      |> Day17.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_17_input.txt")
      |> Day17.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
