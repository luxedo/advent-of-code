defmodule Day{day} do
  @moduledoc """
  Advent Of Code {year} - day {day}
  {url}

  {description_pt1}

  {description_pt2}
  """

  @doc """
      iex> Day{day}.solve_pt1("abc")
      5

      iex> Day{day}.solve_pt1("def")
      4
  """
  def solve_pt1(input) do
    0
  end

  @doc """
      iex> Day{day}.solve_pt2("abc")
      5

      iex> Day{day}.solve_pt2("def")
      4
  """
  def solve_pt2(input) do
    1
  end

  def main do
    ans1 =
      File.read!("../data/day_{day}_input.txt")
      |> Day{day}.solve_pt1

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_{day}_input.txt")
      |> Day{day}.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
