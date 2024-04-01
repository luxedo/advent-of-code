defmodule Year2023Day23 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 23
  Elixir Solution

  Day 23: A Long Walk

  https://adventofcode.com/2023/day/23
  """
  import EsbFireplace

  defmodule Node do
    defstruct [:c, :y, :x, :paths]
  end

  defmodule Path do
    defstruct [:start, :finish, :steps]
  end

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

    def dijkstra(
          heap,
          nodes,
          seen,
          finish
        ) do
      {{rank, node}, heap} = split(heap)

      nodes = Map.delete(nodes, node)

      cond do
        node in seen ->
          dijkstra(heap, nodes, seen, finish)

        node == finish ->
          seen

        true ->
          seen = MapSet.put(seen, {node.y, node.x})

          possible =
            Enum.filter(node.paths, fn %Path{finish: f} ->
              !MapSet.member?(seen, f)
            end)
            |> Enum.map(fn %Path{finish: f, steps: s} ->
              {rank - s, nodes[f]}
            end)

          heap =
            Enum.reduce(possible, heap, fn next, h ->
              Heap.push(h, next)
            end)

          dijkstra(heap, nodes, seen, finish)
      end
    end
  end

  defmodule Trails do
    defstruct [:trails, :height, :width]

    def parse(input, :no_slopes) do
      String.replace(input, ~r/[\^>v<]/, ".") |> parse()
    end

    def parse(input) do
      tiles =
        String.split(input, "\n", trim: true)
        |> Enum.with_index()
        |> Enum.flat_map(fn {line, y} ->
          String.graphemes(line)
          |> Enum.with_index()
          |> Enum.map(fn {c, x} -> {{y, x}, %Node{c: c, y: y, x: x}} end)
        end)
        |> Enum.filter(fn
          {_, %Node{c: "#"}} -> false
          _ -> true
        end)
        |> Map.new()

      start =
        Enum.find(tiles, fn
          {{0, _}, _} -> true
          _ -> false
        end)

      finish =
        Enum.reduce(tiles, nil, fn
          node, nil -> node
          {{y, _}, _}, nt = {{my, _}, _} when my > y -> nt
          node, _ -> node
        end)

      nodes = build_graph(tiles, start, finish)
      start = Enum.find(nodes, fn {pos, _} -> pos == elem(start, 0) end)
      finish = Enum.find(nodes, fn {pos, _} -> pos == elem(finish, 0) end)

      {elem(start, 1), elem(finish, 1), nodes}
    end

    def build_graph(tiles, start, finish) do
      nodes =
        Enum.filter(tiles, fn {{y, x}, _} ->
          neighbors(tiles, y, x) |> Enum.count() >= 3
        end)
        |> Enum.map(fn {pos, _} -> pos end)
        |> MapSet.new()
        |> MapSet.union(MapSet.new([elem(start, 0), elem(finish, 0)]))

      links =
        Enum.flat_map(nodes, fn start_node = {y, x} ->
          neighbors(tiles, y, x)
          |> Enum.map(fn {pos, _} ->
            link_nodes(tiles, nodes, pos, [start_node])
          end)
          |> Enum.filter(&(&1 !== nil))
        end)
        |> MapSet.new()

      nodes =
        Enum.map(nodes, fn pos ->
          {pos,
           tiles[pos]
           |> Map.put(
             :paths,
             Enum.filter(links, fn link ->
               link.start == pos
             end)
             # |> MapSet.new()
           )}
        end)
        |> Map.new()

      nodes
    end

    def neighbors(tiles, y, x) do
      Enum.filter([{y - 1, x}, {y, x + 1}, {y + 1, x}, {y, x - 1}], fn direction ->
        Map.has_key?(tiles, direction)
      end)
      |> Enum.map(fn pos -> {pos, tiles[pos]} end)
    end

    def link_nodes(tiles, nodes, node = {y, x}, seen) do
      next_node =
        neighbors(tiles, y, x)
        |> Enum.filter(fn {_, %Node{c: c, y: ny, x: nx}} ->
          case c do
            "^" -> ny < y
            ">" -> nx > x
            "v" -> ny > y
            "<" -> nx < x
            "." -> true
          end
        end)
        |> Enum.map(fn {{y, x}, _} -> {y, x} end)
        |> MapSet.new()
        |> MapSet.difference(MapSet.new(seen))

      case next_node |> Enum.to_list() do
        # Dead end
        [] ->
          nil

        [nnode] ->
          if nnode in nodes do
            %Path{start: Enum.at(seen, -1), finish: nnode, steps: length(seen) + 1}
            # [nnode, node | seen] |> Enum.reverse()
          else
            link_nodes(tiles, nodes, nnode, [node | seen])
          end
      end
    end

    def connected(start, finish, _, _) when start == finish, do: true
    def connected(_, _, nodes, _) when map_size(nodes) == 0, do: false

    def connected(start, finish, nodes, seen) do
      nodes = Map.delete(nodes, start)
      seen = MapSet.put(seen, {start.y, start.x})

      Enum.reduce_while(start.paths, false, fn %Path{finish: f}, _ ->
        found =
          cond do
            f in seen -> false
            !Map.has_key?(nodes, f) -> false
            connected(nodes[f], finish, nodes, seen) -> true
          end

        if found, do: {:halt, true}, else: {:cont, false}
      end)
    end

    def all_paths(start, finish, _, _) when start == finish, do: [0]
    def all_paths(_, _, nodes, _) when map_size(nodes) == 0, do: nil

    def all_paths(start, finish, nodes, seen) do
      nodes = Map.delete(nodes, {start.y, start.x})

      Enum.filter(start.paths, fn %Path{finish: f} ->
        Map.has_key?(nodes, f)
      end)
      |> Enum.map(fn %Path{finish: f, steps: s} ->
        [s | all_paths(nodes[f], finish, nodes, seen)]
      end)
    end

    def flatten(tree) do
      flatten(tree, [])
    end

    def flatten(value, path) when is_integer(value) do
      [Enum.sum(path ++ [value])]
    end

    def flatten([head | tail], path) do
      Enum.flat_map(tail, &flatten(&1, [head | path]))
    end

    def longest_path(start, finish, nodes) do
      [paths] = all_paths(start, finish, nodes, MapSet.new())
      flatten(paths) |> Enum.max()
    end
  end

  defp solve_pt1(input_data, _args) do
    {start, finish, nodes} = Trails.parse(input_data)
    Trails.longest_path(start, finish, nodes)
  end

  defp solve_pt2(input_data, _args) do
    {start, finish, nodes} = Trails.parse(input_data, :no_slopes)
    Trails.longest_path(start, finish, nodes)
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
