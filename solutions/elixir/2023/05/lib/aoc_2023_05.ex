defmodule Year2023Day05 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 05
  Elixir Solution

  Day 5: If You Give A Seed A Fertilizer

  https://adventofcode.com/2023/day/5
  """
  import EsbFireplace

  defmodule Almanac do
    def parse(input) do
      [seeds | tail] = String.split(input, "\n\n")
      seeds = String.split(seeds, " ") |> List.delete_at(0) |> Enum.map(&String.to_integer/1)

      maps =
        Enum.map(tail, fn t ->
          [name | maps] = String.split(t, "\n", trim: true)

          [from, to] =
            String.replace_trailing(name, " map:", "") |> String.split("-") |> List.delete_at(1)

          map =
            Enum.map(maps, fn m -> String.split(m, " ") |> Enum.map(&String.to_integer/1) end)
            |> Enum.map(fn [dest, source, length] ->
              {source..(source + length - 1), dest - source}
            end)

          {from, %{map: map, to: to}}
        end)
        |> Map.new()

      {seeds, maps}
    end

    def run_seeds(values, key, map) do
      case Map.has_key?(map, key) do
        false ->
          values

        true ->
          m = Map.fetch!(map, key)

          v =
            Enum.map(values, fn v ->
              case Enum.find(m.map, fn {r, _} -> v in r end) do
                nil -> v
                {_, offset} -> v + offset
              end
            end)

          run_seeds(v, m.to, map)
      end
    end

    def remap(range = start..stop, rsrc = src_start..src_stop, offset) do
      if Range.disjoint?(range, rsrc) do
        {[range], []}
      else
        case {start < src_start, stop > src_stop} do
          {true, true} ->
            {[start..(src_start - 1), (src_stop + 1)..stop],
             [(src_start + offset)..(src_stop + offset)]}

          {false, false} ->
            {[], [(start + offset)..(stop + offset)]}

          {true, false} ->
            {[start..(src_start - 1)], [(src_start + offset)..(stop + offset)]}

          {false, true} ->
            {[(src_stop + 1)..stop], [(start + offset)..(src_stop + offset)]}
        end
      end
    end

    def step(map, items, key) do
      {keep, change} =
        Enum.reduce(map[key].map, {items, []}, fn {rsrc, offset}, {src, dst} ->
          {nsrc, ndst} = Enum.map(src, &remap(&1, rsrc, offset)) |> Enum.unzip()
          {List.flatten(nsrc), List.flatten(ndst) ++ dst}
        end)

      {map[key].to, keep ++ change}
    end

    def location(map, items, key) do
      case step(map, items, key) do
        {"location", items} -> items
        {key, items} -> location(map, items, key)
      end
    end

    def location(map, seeds) do
      location(map, seeds, "seed")
    end
  end

  def solve_pt1(input_data, _args) do
    {seeds, map} = Almanac.parse(input_data)
    Almanac.run_seeds(seeds, "seed", map) |> Enum.min()
  end

  def solve_pt2(input_data, _args) do
    {seeds, map} = Almanac.parse(input_data)

    seeds =
      Enum.chunk_every(seeds, 2)
      |> Enum.map(fn [a, b] -> a..(a + b) end)

    Almanac.location(map, seeds) |> Enum.map(fn start.._ -> start end) |> Enum.min()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
