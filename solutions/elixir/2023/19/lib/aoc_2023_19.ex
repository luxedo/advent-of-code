defmodule Year2023Day19 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 19
  Elixir Solution

  Day 19: Aplenty

  https://adventofcode.com/2023/day/19
  """
  import EsbFireplace

  defmodule Rule do
    defstruct [:target, key: nil, cmp: nil, val: nil]

    def from_string(string) do
      case String.split(string, ":") do
        [r, t] ->
          key =
            case String.first(r) do
              "x" -> :x
              "m" -> :m
              "a" -> :a
              "s" -> :s
            end

          cmp =
            case String.at(r, 1) do
              ">" -> :>
              "<" -> :<
            end

          val = String.slice(r, 2..-1//1) |> String.to_integer()

          %Rule{target: t, key: key, cmp: cmp, val: val}

        [t] ->
          %Rule{target: t}
      end
    end
  end

  defmodule RuleSet do
    def from_string(string) do
      [name, rules] = String.trim_trailing(string, "}") |> String.split("{")
      rules = String.split(rules, ",") |> Enum.map(&Rule.from_string/1)
      {name, rules}
    end
  end

  defmodule Parts do
    defstruct x: 0, m: 0, a: 0, s: 0

    def from_string(string) do
      p =
        String.trim_trailing(string, "}")
        |> String.trim_leading("{")
        |> String.split(",")
        |> Enum.map(fn s ->
          [k, v] = String.split(s, "=")
          {k, String.to_integer(v)}
        end)
        |> Map.new()

      %Parts{
        x: Map.get(p, "x", 0),
        m: Map.get(p, "m", 0),
        a: Map.get(p, "a", 0),
        s: Map.get(p, "s", 0)
      }
    end
  end

  defmodule Workflow do
    def run(
          rules,
          [%Rule{target: target, key: key, cmp: cmp, val: val} | tail],
          part
        ) do
      arg =
        if key == nil do
          target
        else
          case cmp do
            :< -> if Map.get(part, key) < val, do: target, else: tail
            :> -> if Map.get(part, key) > val, do: target, else: tail
          end
        end

      run(rules, arg, part)
    end

    def run(rules, name, part) do
      case name do
        "A" -> :A
        "R" -> :R
        _ -> run(rules, rules[name], part)
      end
    end

    def run(rules, parts) do
      Enum.map(parts, &run(rules, "in", &1))
    end

    def run_ranges(
          rules,
          [%Rule{target: target, key: key, cmp: cmp, val: val} | tail],
          parts
        ) do
      if key == nil do
        run_ranges(rules, target, parts)
      else
        Enum.flat_map(parts, fn part ->
          start..stop = Map.get(part, key)

          {range_true, range_false} =
            case cmp do
              :< -> {start..(val - 1), val..stop}
              :> -> {(val + 1)..stop, start..val}
            end

          run_ranges(rules, target, [Map.replace!(part, key, range_true)]) ++
            run_ranges(rules, tail, [Map.replace!(part, key, range_false)])
        end)
      end
    end

    def run_ranges(rules, name, parts) do
      if name not in Map.keys(rules) do
        [{name, parts}]
      else
        run_ranges(rules, rules[name], parts)
      end
    end

    def run_ranges(rules, range) do
      run_ranges(rules, "in", [%Parts{x: range, m: range, a: range, s: range}])
      |> Enum.map(fn {s, [r]} -> {s, r} end)
    end
  end

  def parse(input) do
    [rules, parts] = String.split(input, "\n\n") |> Enum.map(&String.split(&1, "\n", trim: true))
    rules = Enum.map(rules, &RuleSet.from_string/1) |> Map.new()
    parts = Enum.map(parts, &Parts.from_string/1)
    {rules, parts}
  end

  defp solve_pt1(input_data, _args) do
    {rules, parts} = parse(input_data)
    status = Workflow.run(rules, parts)

    Enum.zip(parts, status)
    |> Enum.filter(fn {_, s} -> s == :A end)
    |> Enum.map(fn {%Parts{x: x, m: m, a: a, s: s}, _} ->
      x + m + a + s
    end)
    |> Enum.sum()
  end

  defp solve_pt2(input_data, _args) do
    {rules, _} = parse(input_data)

    Workflow.run_ranges(rules, 1..4000)
    |> Enum.filter(fn {s, _} -> s == "A" end)
    |> Enum.map(fn {_,
                    %Parts{
                      x: x0..x1,
                      m: m0..m1,
                      a: a0..a1,
                      s: s0..s1
                    }} ->
      (x1 - x0 + 1) * (m1 - m0 + 1) * (a1 - a0 + 1) * (s1 - s0 + 1)
    end)
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
