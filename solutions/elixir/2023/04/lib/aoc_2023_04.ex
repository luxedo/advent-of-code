defmodule Year2023Day04 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 04
  Elixir Solution

  Day 4: Scratchcards

  https://adventofcode.com/2023/day/4
  """
  import EsbFireplace

  @base 2

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      String.split(line, ":", trim: true)
    end)
    |> Enum.map(fn [card, values] ->
      card = String.replace_leading(card, "Card", "") |> String.trim() |> String.to_integer()

      [winning, yours] =
        String.split(values, " | ")
        |> Enum.map(fn m -> String.split(m, " ", trim: true) |> Enum.map(&String.to_integer/1) end)

      {card, winning, yours}
    end)
  end

  def score_cards(input) do
    {hits, scores} =
      parse(input)
      |> Enum.map(fn {card, winning, yours} ->
        hits =
          Enum.map(yours, fn n -> (Enum.member?(winning, n) && 1) || 0 end)
          |> Enum.sum()

        scores =
          Integer.pow(@base, hits)
          |> Integer.floor_div(@base)

        {{card, hits}, {card, scores}}
      end)
      |> Enum.unzip()

    {hits |> Map.new(), scores |> Map.new()}
  end

  def solve_pt1(input_data, _args) do
    {_, scores} = score_cards(input_data)
    scores |> Map.values() |> Enum.sum()
  end

  def solve_pt2(input_data, _args) do
    {hits, _} = score_cards(input_data)

    Enum.reduce(
      hits |> Enum.sort(),
      Enum.map(hits, &{elem(&1, 0), 1}) |> Map.new(),
      fn
        {_, 0}, acc ->
          acc

        {card, h}, acc ->
          repeats = Map.fetch!(acc, card)

          Enum.reduce((card + 1)..(card + h), acc, fn c, acc ->
            Map.update!(acc, c, &(&1 + repeats))
          end)
      end
    )
    |> Map.values()
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
