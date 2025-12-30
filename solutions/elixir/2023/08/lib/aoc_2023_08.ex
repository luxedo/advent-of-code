defmodule Year2023Day08 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 08
  Elixir Solution

  Day 8: Haunted Wasteland

  https://adventofcode.com/2023/day/8
  """
  import EsbFireplace

  @start_pt1 "AAA"
  @final_pt1 "ZZZ"
  @start_pt2 "A"
  @final_pt2 "Z"

  def parse(input) do
    [instructions, map] = String.split(input, "\n\n")

    map =
      String.split(map, "\n", trim: true)
      |> Enum.map(fn line ->
        [from, to] = String.split(line, " = ")
        [left, right] = String.replace(to, ~r/[()]/, "") |> String.split(", ")
        {from, %{"L" => left, "R" => right}}
      end)
      |> Map.new()

    {String.graphemes(instructions), map}
  end

  def next_node(map, node, side) do
    Map.fetch!(map, node) |> Map.fetch!(side)
  end

  def count_cycles(instructions, map, starting_nodes, end_nodes) do
    nodes = Enum.map(starting_nodes, &{&1, 0, false})

    Stream.cycle(instructions)
    |> Enum.reduce_while(nodes, fn side, nodes ->
      case Enum.all?(nodes, fn {_, _, finished} -> finished end) do
        true ->
          {:halt, nodes}

        false ->
          {:cont,
           Enum.map(nodes, fn n = {node, counter, finished} ->
             case finished do
               true ->
                 n

               false ->
                 next = next_node(map, node, side)
                 finished = Enum.any?(end_nodes, &String.ends_with?(next, &1))
                 {next, counter + 1, finished}
             end
           end)}
      end
    end)
  end

  def lcm(0, 0), do: 0
  def lcm(a, b), do: Kernel.trunc(a * b / Integer.gcd(a, b))

  def solve_pt1(input_data, _args) do
    {instructions, map} = parse(input_data)

    starting_nodes = [@start_pt1]
    final_nodes = [@final_pt1]

    [{_, counter, _}] = count_cycles(instructions, map, starting_nodes, final_nodes)
    counter
  end

  def solve_pt2(input_data, _args) do
    {instructions, map} = parse(input_data)

    starting_nodes = Map.keys(map) |> Enum.filter(fn k -> String.ends_with?(k, @start_pt2) end)
    final_nodes = [@final_pt2]

    count_cycles(instructions, map, starting_nodes, final_nodes)
    |> Enum.map(fn {_, c, _} -> c end)
    |> Enum.reduce(1, fn cur, acc -> lcm(cur, acc) end)
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
