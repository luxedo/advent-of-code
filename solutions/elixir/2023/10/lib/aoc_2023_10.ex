defmodule Year2023Day10 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 10
  Elixir Solution

  Day 10: Pipe Maze

  https://adventofcode.com/2023/day/10
  """
  import EsbFireplace

  require Integer

  @directions %{
    n: {-1, 0},
    e: {0, 1},
    s: {1, 0},
    w: {0, -1}
  }
  @opposite_directions %{
    n: {1, 0},
    e: {0, -1},
    s: {-1, 0},
    w: {0, 1}
  }

  @change_perspective %{
    n: :s,
    e: :w,
    s: :n,
    w: :e
  }

  @pipe_map %{
    "|" => %{
      value: "|",
      n: {:ok, :s},
      e: {:err},
      s: {:ok, :n},
      w: {:err}
    },
    "-" => %{
      value: "-",
      n: {:err},
      e: {:ok, :w},
      s: {:err},
      w: {:ok, :e}
    },
    "F" => %{
      value: "F",
      n: {:err},
      e: {:ok, :s},
      s: {:ok, :e},
      w: {:err}
    },
    "7" => %{
      value: "7",
      n: {:err},
      e: {:err},
      s: {:ok, :w},
      w: {:ok, :s}
    },
    "J" => %{
      value: "J",
      n: {:ok, :w},
      e: {:err},
      s: {:err},
      w: {:ok, :n}
    },
    "L" => %{
      value: "L",
      n: {:ok, :e},
      e: {:ok, :n},
      s: {:err},
      w: {:err}
    },
    "S" => %{
      value: "S",
      n: {:err},
      e: {:err},
      s: {:err},
      w: {:err}
    },
    "." => %{
      value: ".",
      n: {:err},
      e: {:err},
      s: {:err},
      w: {:err}
    }
  }

  def print_pipes(input) do
    Enum.map_join(input, "\n", fn line -> Enum.map_join(line, & &1.value) end) |> IO.puts()
  end

  def parse(input) do
    pipes =
      String.split(input, "\n", trim: true)
      |> Stream.with_index()
      |> Enum.map(fn {line, y} ->
        String.graphemes(line)
        |> Stream.with_index()
        |> Enum.map(fn {p, x} ->
          Map.fetch!(@pipe_map, p) |> Map.merge(%{y: y, x: x})
        end)
      end)

    start = List.flatten(pipes) |> Enum.find(&(&1.value == "S"))
    {pipes, start}
  end

  def iter_pipes(pipes, incoming, pipe) do
    {:ok, going} = pipe[incoming]
    {dy, dx} = @directions[going]
    next_pipe = Enum.at(pipes, pipe.y + dy) |> Enum.at(pipe.x + dx)
    {@change_perspective[going], next_pipe}
  end

  def find_cycle(pipes, start, incoming, pipe) do
    if start == pipe do
      [start]
    else
      {going, next_pipe} = iter_pipes(pipes, incoming, pipe)
      [pipe | find_cycle(pipes, start, going, next_pipe)]
    end
  end

  def find_cycle(pipes, start) do
    [{incoming, next_pipe}, _] =
      Enum.map(@opposite_directions, fn {d, {dy, dx}} ->
        {d, Enum.at(pipes, start.y + dy) |> Enum.at(start.x + dx)}
      end)
      |> Enum.filter(fn {d, p} -> Map.fetch!(p, d) != {:err} end)

    find_cycle(pipes, start, incoming, next_pipe)
  end

  def clamp_angle(angle) do
    cond do
      angle > :math.pi() -> clamp_angle(angle - 2 * :math.pi())
      angle < -:math.pi() -> clamp_angle(angle + 2 * :math.pi())
      true -> angle
    end
  end

  def winding_number({y, x}, polygon) do
    angles = Enum.map(polygon, fn {py, px} -> :math.atan2(y - py, x - px) end)

    ((Enum.zip(angles, [List.last(angles) | angles])
      |> Enum.map(fn {prev, cur} -> clamp_angle(cur - prev) end)
      |> Enum.sum()) / (2 * :math.pi()))
    |> Kernel.round()
  end

  def solve_pt1(input_data, _args) do
    {pipes, start} = parse(input_data)
    ((find_cycle(pipes, start) |> Kernel.length()) + 1) |> Integer.floor_div(2)
  end

  def solve_pt2(input_data, _args) do
    {pipes, start} = parse(input_data)
    polygon = find_cycle(pipes, start) |> Enum.map(fn %{y: y, x: x} -> {y, x} end)

    {maxy, maxx} = {Kernel.length(pipes) - 1, Kernel.length(Enum.at(pipes, 0)) - 1}

    List.flatten(pipes)
    |> Enum.map(fn %{y: y, x: x} -> {y, x} end)
    |> Enum.filter(fn {y, x} -> x > 0 && x < maxx && y > 0 && y < maxy end)
    |> Enum.filter(&(!Enum.member?(polygon, &1)))
    |> Enum.filter(fn p -> winding_number(p, polygon) |> Integer.is_odd() end)
    |> Kernel.length()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
