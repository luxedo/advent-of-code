defmodule Year2023Day21 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 21
  Elixir Solution

  Day 21: Step Counter

  https://adventofcode.com/2023/day/21
  """
  import EsbFireplace

  defmodule Plot do
    defstruct [:vec, plots: [], border: []]
  end

  defmodule Garden do
    defstruct [:garden, :border, :height, :width]

    def find_adjacent(garden) do
      Enum.map(garden, fn {_, p} ->
        vec = {y, x} = p.vec

        plots =
          Enum.map([{y - 1, x}, {y, x + 1}, {y + 1, x}, {y, x - 1}], fn vec ->
            Map.get(garden, vec)
          end)
          |> Enum.filter(&(&1 != nil))
          |> Enum.filter(fn
            %Plot{} -> true
            _ -> false
          end)
          |> Enum.map(& &1.vec)

        {vec, plots}
      end)
      |> Map.new()
    end

    def parse(input) do
      garden =
        String.split(input, "\n", trim: true)
        |> Enum.with_index()
        |> Enum.flat_map(fn {line, y} ->
          String.graphemes(line)
          |> Enum.with_index()
          |> Enum.map(fn {c, x} ->
            case c do
              "." -> {{y, x}, %Plot{vec: {y, x}}}
              "S" -> {{y, x}, :start}
              "#" -> nil
            end
          end)
        end)
        |> Enum.filter(&(&1 !== nil))
        |> Map.new()

      {start, _} = Enum.find(garden, fn {_, val} -> val == :start end)

      garden = Map.replace!(garden, start, %Plot{vec: start}) |> find_adjacent()

      {height, width} =
        Enum.reduce(garden, {0, 0}, fn {{y, x}, _}, {ay, ax} ->
          {if(y > ay, do: y, else: ay), if(x > ax, do: x, else: ax)}
        end)

      garden = %Garden{garden: garden, height: height, width: width}
      border = border_plots(garden)

      {start, Map.replace!(garden, :border, border)}
    end

    def border_plots(%Garden{garden: garden, height: height, width: width}) do
      Enum.map(garden, fn {{y, x}, _} ->
        y_border =
          case y do
            0 -> [{{-1, 0}, {height, x}}]
            h when h == height -> [{{1, 0}, {0, x}}]
            _ -> []
          end

        x_border =
          case x do
            0 -> [{{0, -1}, {y, width}}]
            w when w == width -> [{{0, 1}, {y, 0}}]
            _ -> []
          end

        {{y, x}, y_border ++ x_border}
      end)
      |> Enum.filter(fn
        {_, []} -> false
        _ -> true
      end)
      |> Map.new()
    end

    def get(garden = %{}, p), do: Map.fetch!(garden, p)

    def get_all(garden = %Garden{}, ps) when is_list(ps), do: Enum.map(ps, &get(garden, &1))

    def step_plots(%Garden{garden: garden}, steps = %MapSet{}) do
      Enum.flat_map(steps, fn ps -> Map.fetch!(garden, ps) end)
      |> MapSet.new()
    end

    def step_plots(
          %Garden{garden: garden, border: border, height: height, width: width},
          positions
        ) do
      # Compute centers
      centers =
        Enum.map(positions, fn {loc, steps} ->
          new_pos =
            if cache = Process.get({:c, steps}) do
              cache
            else
              new_steps =
                Enum.flat_map(steps, fn ps -> Map.fetch!(garden, ps) end)
                |> MapSet.new()

              Process.put({:c, steps}, new_steps)
              new_steps
            end

          {loc, new_pos}
        end)
        |> Map.new()

      borders =
        Enum.flat_map(positions, fn {{ly, lx}, steps} ->
          steps =
            Enum.filter(steps, fn
              {0, _} -> true
              {_, 0} -> true
              {h, _} when h == height -> true
              {_, w} when w == width -> true
              _ -> false
            end)
            |> MapSet.new()

          new_pos =
            if cache = Process.get({:b, steps}) do
              cache
            else
              new_steps =
                Enum.flat_map(steps, fn ps -> Map.fetch!(border, ps) end)
                |> Enum.reduce(%{}, fn {{nly, nlx}, p}, acc ->
                  Map.update(acc, {nly, nlx}, MapSet.new([p]), &MapSet.put(&1, p))
                end)
                |> Map.new()

              Process.put({:b, steps}, new_steps)
              new_steps
            end

          Enum.map(new_pos, fn {{nly, nlx}, p} ->
            {{ly + nly, lx + nlx}, p}
          end)
        end)
        |> Enum.reduce(%{}, fn {sector, ms}, acc ->
          Map.update(acc, sector, ms, &MapSet.union(&1, ms))
        end)

      Map.merge(borders, centers, fn
        _, v1 = %MapSet{}, v2 = %MapSet{} -> MapSet.union(v1, v2)
        _, v1 = %MapSet{}, _ -> v1
        _, _, v2 = %MapSet{} -> v2
      end)
    end
  end

  def find_sequence(seq) when is_list(seq) do
    if Enum.all?(seq, &(&1 == 0)) do
      [seq]
    else
      new_seq =
        Enum.reduce(Enum.zip(seq, List.delete_at(seq, 0)), [], fn {prev, cur}, acc ->
          [cur - prev | acc]
        end)
        |> Enum.reverse()

      [seq | find_sequence(new_seq)]
    end
  end

  def next_in_sequence(seq) when is_list(seq) do
    Enum.reverse(seq)
    |> Enum.reduce(0, fn cur, acc ->
      List.last(cur) + acc
    end)
  end

  defp solve_pt1(input_data, args) do
    n =
      case args do
        [value] -> String.to_integer(value)
        _ -> 64
      end

    {start, garden} = Garden.parse(input_data)
    start = %{{0, 0} => MapSet.new([start])}

    [plots | _] =
      Enum.reduce(1..n, {start, [1]}, fn _, {acc, c} ->
        acc = Garden.step_plots(garden, acc)
        nc = Enum.flat_map(acc, fn {_, v} -> v end) |> Enum.count()
        {acc, [nc | c]}
      end)
      |> Kernel.elem(1)

    plots
  end

  defp solve_pt2(input_data, args) do
    n =
      case args do
        [value] -> String.to_integer(value)
        _ -> 26_501_365
      end

    {start, garden} = Garden.parse(input_data)
    start = %{{0, 0} => MapSet.new([start])}

    side = garden.height + 1
    iters = trunc(side / 2) + 8 * side

    {plots, ixs} =
      Enum.reduce(1..iters, {start, [1]}, fn _, {acc, c} ->
        acc = Garden.step_plots(garden, acc)
        nc = Enum.flat_map(acc, fn {_, v} -> v end) |> Enum.count()
        {acc, [nc | c]}
      end)
      |> Kernel.elem(1)
      |> Enum.reverse()
      |> Enum.with_index()
      |> Enum.filter(fn {_, i} -> rem(i + trunc(side / 2 + 1), side) == 0 end)
      |> Enum.unzip()

    s = trunc((n - trunc(side / 2 + 1)) / side) - length(plots) + 2

    [{plots, _idx} | _] =
      Enum.reduce(1..s, {plots, ixs}, fn _, {acc, ixs} ->
        b = find_sequence(acc)
        {next, inext} = {next_in_sequence(b), List.last(ixs) + side}
        {Enum.slice(acc, -4..-1) ++ [next], Enum.slice(ixs, -4..-1) ++ [inext]}
      end)
      |> Tuple.to_list()
      |> Enum.zip()
      |> Enum.reverse()

    # 608152828731262
    plots
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
