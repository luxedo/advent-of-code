defmodule Year2023Day25 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 25
  Elixir Solution

  Day 25: Snowverload

  https://adventofcode.com/2023/day/25
  """
  import EsbFireplace

  defmodule Dijkstra do
    def shortest_path(graph, start, target) do
      distances = Map.keys(graph) |> Enum.map(&{&1, :inf}) |> Map.new() |> Map.replace!(start, 0)
      previous = Map.keys(graph) |> Enum.map(&{&1, nil}) |> Map.new()
      unvisited = Map.keys(graph) |> MapSet.new()

      previous = dijkstra(graph, distances, previous, unvisited, target)
      build_path(previous, target)
    end

    defp dijkstra(graph, distances, previous, unvisited, target) do
      current =
        Enum.min_by(unvisited, fn node ->
          Map.get(distances, node)
        end)

      neighbors = Map.get(graph, current, MapSet.new())
      next_distance = Map.get(distances, current) + 1

      {distances, previous} =
        Enum.reduce(neighbors, {distances, previous}, fn neighbor, {distances, previous} ->
          if next_distance < Map.get(distances, neighbor) do
            {Map.replace!(distances, neighbor, next_distance),
             Map.replace!(previous, neighbor, current)}
          else
            {distances, previous}
          end
        end)

      if current == target do
        previous
      else
        dijkstra(graph, distances, previous, MapSet.delete(unvisited, current), target)
      end
    end

    defp build_path(_previous, nil), do: []

    defp build_path(previous, vertex),
      do: build_path(previous, Map.get(previous, vertex)) ++ [vertex]
  end

  defmodule Wires do
    def parse(input) do
      parsed =
        String.split(input, "\n", trim: true)
        |> Enum.map(fn line ->
          [src, dst] = String.split(line, ": ")
          {src, String.split(dst)}
          # %Component{name: src, links: String.split(dst) |> MapSet.new()}
        end)

      all_nodes = Enum.flat_map(parsed, fn {src, dst} -> [src | dst] end) |> MapSet.new()

      Enum.map(all_nodes, fn name ->
        {name,
         Enum.reduce(parsed, [], fn
           {src, dst}, acc when src == name -> dst ++ acc
           {src, dst}, acc -> if name in dst, do: [src | acc], else: acc
         end)
         |> MapSet.new()}
      end)
      |> Map.new()
    end

    def remove(graph, name0, name1) do
      Map.update!(graph, name0, &MapSet.delete(&1, name1))
      |> Map.update!(name1, &MapSet.delete(&1, name0))
    end

    def split_graph(graph, node) do
      con = connected(graph, node, MapSet.new())
      [con] ++ split_graph(Map.drop(graph, MapSet.to_list(con)))
    end

    def split_graph(graph) when map_size(graph) == 0, do: []

    def split_graph(graph) do
      {node, _} = Map.to_list(graph) |> hd()
      split_graph(graph, node)
    end

    defp connected(graph, node, seen) do
      Enum.reduce(Map.get(graph, node, []), MapSet.put(seen, node), fn next, acc ->
        if next in acc, do: acc, else: connected(graph, next, acc)
      end)
    end

    def bisect_graph(graph) do
      nodes = Map.keys(graph)

      random_paths =
        for(
          _ <- 0..50,
          do: Dijkstra.shortest_path(graph, Enum.random(nodes), Enum.random(nodes))
        )
        |> Enum.filter(&(Enum.count(&1) > 1))

      candidates =
        Enum.flat_map(random_paths, fn path ->
          Enum.zip(path, Enum.slice(path, 1..-1//1))
          |> Enum.map(fn v -> Tuple.to_list(v) |> Enum.sort() |> List.to_tuple() end)
        end)
        |> Enum.frequencies()
        |> Map.to_list()
        |> Enum.sort_by(fn {_, v} -> -v end)
        |> Enum.slice(0..2)
        |> Enum.map(fn {link, _} -> link end)

      split =
        Enum.reduce(candidates, graph, fn {node0, node1}, acc ->
          Wires.remove(acc, node0, node1)
        end)
        |> Wires.split_graph()

      if Enum.count(split) > 1 do
        Enum.map(split, &MapSet.size/1) |> Enum.product()
      else
        bisect_graph(graph)
      end
    end
  end

  defp solve_pt1(input_data, _args) do
    Wires.parse(input_data) |> Wires.bisect_graph()
  end

  defp solve_pt2(_input_data, _args) do
    # Solve pt2
    :congratulations
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
