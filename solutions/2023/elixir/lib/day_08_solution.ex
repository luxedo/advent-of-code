defmodule Day08 do
  @moduledoc """
  Advent Of Code 2023 - day 08
  https://adventofcode.com/2023/day/8

    --- Day 8: Haunted Wasteland ---
  You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching.
  When you turn to warn the Elf, she disappears before your eyes! To be fair, she had just finished
  warning you about ghosts a few minutes ago.

  One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle
  input) about how to navigate the desert. At least, you're pretty sure that's what they are; one of
  the documents contains a list of left/right instructions, and the rest of the documents seem to
  describe some kind of network of labeled nodes.

  It seems like you're meant to use the left/right instructions to navigate the network. Perhaps if
  you have the camel follow the same instructions, you can escape the haunted wasteland!

  After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like AAA is where
  you are now, and you have to follow the left/right instructions until you reach ZZZ.

  This format defines each node of the network individually. For example:
  RL

  AAA = (BBB, CCC)
  BBB = (DDD, EEE)
  CCC = (ZZZ, GGG)
  DDD = (DDD, DDD)
  EEE = (EEE, EEE)
  GGG = (GGG, GGG)
  ZZZ = (ZZZ, ZZZ)


  Starting with AAA, you need to look up the next element based on the next left/right instruction
  in your input. In this example, start with AAA and go right (R) by choosing the right element of
  AAA, CCC. Then, L means to choose the left element of CCC, ZZZ. By following the left/right
  instructions, you reach ZZZ in 2 steps.

  Of course, you might not find ZZZ right away. If you run out of left/right instructions, repeat
  the whole sequence of instructions as necessary: RL really means RLRLRLRLRLRLRLRL... and so on.
  For example, here is a situation that takes 6 steps to reach ZZZ:
  LLR

  AAA = (BBB, BBB)
  BBB = (AAA, ZZZ)
  ZZZ = (ZZZ, ZZZ)


  Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?

  --- Part Two ---
  The sandstorm is upon you and you aren't any closer to escaping the wasteland. You had the camel
  follow the instructions, but you've barely left your starting position. It's going to take
  significantly more steps to escape!

  What if the map isn't for people - what if the map is for ghosts? Are ghosts even bound by the
  laws of spacetime? Only one way to find out.

  After examining the maps a bit longer, your attention is drawn to a curious fact: the number of
  nodes with names ending in A is equal to the number ending in Z! If you were a ghost, you'd
  probably just start at every node that ends with A and follow all of the paths at the same time
  until they all simultaneously end up at nodes that end with Z.

  For example:
  LR

  11A = (11B, XXX)
  11B = (XXX, 11Z)
  11Z = (11B, XXX)
  22A = (22B, XXX)
  22B = (22C, 22C)
  22C = (22Z, 22Z)
  22Z = (22B, 22B)
  XXX = (XXX, XXX)


  Here, there are two starting nodes, 11A and 22A (because they both end with A). As you follow each
  left/right instruction, use that instruction to simultaneously navigate away from both nodes
  you're currently on. Repeat this process until all of the nodes you're currently on end with Z.
  (If only some of the nodes you're on end with Z, they act like any other node and you continue as
  normal.) In this example, you would proceed as follows:

  Step 0: You are at 11A and 22A.
  Step 1: You choose all of the left paths, leading you to 11B and 22B.
  Step 2: You choose all of the right paths, leading you to 11Z and 22C.
  Step 3: You choose all of the left paths, leading you to 11B and 22Z.
  Step 4: You choose all of the right paths, leading you to 11Z and 22B.
  Step 5: You choose all of the left paths, leading you to 11B and 22C.
  Step 6: You choose all of the right paths, leading you to 11Z and 22Z.


  So, in this example, you end up entirely on nodes that end in Z after 6 steps.

  Simultaneously start on every node that ends with A. How many steps does it take before you're
  only on nodes that end with Z?
  """

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
    nodes = Enum.map(starting_nodes, &({&1, 0, false}))

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

  @doc """
       iex> Day08.solve_pt1(\"""
       ...>RL
       ...>
       ...>AAA = (BBB, CCC)
       ...>BBB = (DDD, EEE)
       ...>CCC = (ZZZ, GGG)
       ...>DDD = (DDD, DDD)
       ...>EEE = (EEE, EEE)
       ...>GGG = (GGG, GGG)
       ...>ZZZ = (ZZZ, ZZZ)
       ...>\""")
       2

       iex> Day08.solve_pt1(\"""
       ...>LLR
       ...>
       ...>AAA = (BBB, BBB)
       ...>BBB = (AAA, ZZZ)
       ...>ZZZ = (ZZZ, ZZZ)
       ...>\""")
       6


  """
  def solve_pt1(input) do
    {instructions, map} = parse(input)

    starting_nodes = [@start_pt1]
    final_nodes = [@final_pt1]

    [{_, counter, _}] = count_cycles(instructions, map, starting_nodes, final_nodes)
    counter
  end

  @doc """
       iex> Day08.solve_pt2(\"""
       ...>LR
       ...>
       ...>11A = (11B, XXX)
       ...>11B = (XXX, 11Z)
       ...>11Z = (11B, XXX)
       ...>22A = (22B, XXX)
       ...>22B = (22C, 22C)
       ...>22C = (22Z, 22Z)
       ...>22Z = (22B, 22B)
       ...>XXX = (XXX, XXX)
       ...>\""")
       6
  """
  def solve_pt2(input) do
    {instructions, map} = parse(input)

    starting_nodes = Map.keys(map) |> Enum.filter(fn k -> String.ends_with?(k, @start_pt2) end)
    final_nodes = [@final_pt2]

    count_cycles(instructions, map, starting_nodes, final_nodes)
    |> Enum.map(fn {_, c, _} -> c end)
    |> Enum.reduce(1, fn cur, acc -> lcm(cur, acc) end)
  end

  def main do
    ans1 =
      File.read!("../data/day_08_input.txt")
      |> Day08.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_08_input.txt")
      |> Day08.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
