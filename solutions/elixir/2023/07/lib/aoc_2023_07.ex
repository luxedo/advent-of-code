defmodule Year2023Day07 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 07
  Elixir Solution

  Day 7: Camel Cards

  https://adventofcode.com/2023/day/7
  """
  import EsbFireplace

  @card_values %{
    "A" => 14,
    "K" => 13,
    "Q" => 12,
    "J" => 11,
    "T" => 10,
    "9" => 9,
    "8" => 8,
    "7" => 7,
    "6" => 6,
    "5" => 5,
    "4" => 4,
    "3" => 3,
    "2" => 2
  }
  @card_values_w_joker %{
    "A" => 14,
    "K" => 13,
    "Q" => 12,
    "T" => 10,
    "9" => 9,
    "8" => 8,
    "7" => 7,
    "6" => 6,
    "5" => 5,
    "4" => 4,
    "3" => 3,
    "2" => 2,
    "J" => 1
  }
  @five_of_a_kind_value 6
  @four_of_a_kind_value 5
  @full_house_value 4
  @three_of_a_kind_value 3
  @two_pair_value 2
  @one_pair_value 1
  @high_card_value 0

  def parse(input) do
    String.split(input, "\n", trim: true)
    |> Enum.map(fn line ->
      [hand, bid] = String.split(line, " ")
      hand = String.graphemes(hand)
      {hand, Enum.frequencies(hand), String.to_integer(bid)}
    end)
  end

  def five_of_a_kind([5]), do: true
  def five_of_a_kind(_), do: false

  def four_of_a_kind([4 | _]), do: true
  def four_of_a_kind(_), do: false

  def full_house([3, 2 | _]), do: true
  def full_house(_), do: false

  def three_of_a_kind([3 | _]), do: true
  def three_of_a_kind(_), do: false

  def two_pair([2, 2 | _]), do: true
  def two_pair(_), do: false

  def one_pair([2 | _]), do: true
  def one_pair(_), do: false

  def hand_rank(counts) do
    values = Map.values(counts) |> Enum.sort(:desc)

    cond do
      five_of_a_kind(values) -> @five_of_a_kind_value
      four_of_a_kind(values) -> @four_of_a_kind_value
      full_house(values) -> @full_house_value
      three_of_a_kind(values) -> @three_of_a_kind_value
      two_pair(values) -> @two_pair_value
      one_pair(values) -> @one_pair_value
      true -> @high_card_value
    end
  end

  def hand_rank(counts, :joker) do
    case counts do
      %{"J" => 5} ->
        hand_rank(counts)

      %{"J" => value} ->
        counts_jokerless =
          if value == 1, do: Map.delete(counts, "J"), else: Map.put(counts, "J", value - 1)

        Map.delete(counts, "J")
        |> Enum.map(fn {k, v} ->
          Map.put(counts_jokerless, k, v + 1) |> hand_rank(:joker)
        end)
        |> Enum.max()

      _ ->
        hand_rank(counts)
    end
  end

  def highest(hand0, hand1, values) do
    {h0, h1} = Enum.find(Enum.zip(hand0, hand1), fn {h0, h1} -> h0 != h1 end)
    Map.fetch!(values, h0) < Map.fetch!(values, h1)
  end

  def sort_games(games) do
    Enum.sort(games, fn {hand0, counts0, _}, {hand1, counts1, _} ->
      if (r0 = hand_rank(counts0)) == (r1 = hand_rank(counts1)) do
        highest(hand0, hand1, @card_values)
      else
        {hand0, hand1, r0, r1}
        r0 < r1
      end
    end)
  end

  def sort_games(games, :joker) do
    Enum.sort(games, fn {hand0, counts0, _}, {hand1, counts1, _} ->
      if (r0 = hand_rank(counts0, :joker)) == (r1 = hand_rank(counts1, :joker)) do
        highest(hand0, hand1, @card_values_w_joker)
      else
        {hand0, hand1, r0, r1}
        r0 < r1
      end
    end)
  end

  def solve_pt1(input_data, _args) do
    games = parse(input_data)

    sort_games(games)
    |> Stream.with_index()
    |> Enum.map(fn {{_, _, b}, i} -> (i + 1) * b end)
    |> Enum.sum()
  end

  def solve_pt2(input_data, _args) do
    games = parse(input_data)

    sort_games(games, :joker)
    |> Stream.with_index()
    |> Enum.map(fn {{_, _, b}, i} -> (i + 1) * b end)
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
