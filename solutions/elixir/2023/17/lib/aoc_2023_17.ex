defmodule Year2023Day17 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 17
  Elixir Solution

  Day 17: Clumsy Crucible

  https://adventofcode.com/2023/day/17
  """
  import EsbFireplace

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

  defp solve_pt1(input_data, _args) do
    city = City.new(input_data)
    {start, finish} = {{0, 0}, {city.height - 1, city.width - 1}}
    crucible = {1, 3}
    find_least_heat_path(city, start, finish, crucible)
  end

  defp solve_pt2(input_data, _args) do
    city = City.new(input_data)
    {start, finish} = {{0, 0}, {city.height - 1, city.width - 1}}
    crucible = {4, 10}
    find_least_heat_path(city, start, finish, crucible)
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
