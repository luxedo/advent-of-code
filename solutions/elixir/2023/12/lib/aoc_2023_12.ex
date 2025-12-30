defmodule Year2023Day12 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 12
  Elixir Solution

  Day 12: Hot Springs

  https://adventofcode.com/2023/day/12
  """
  import EsbFireplace

  defmodule Spring do
    defstruct [:springs, :records, :unknown, :dmg, :dmg_t]

    def process_springs(s) do
      String.graphemes(s)
    end

    def parse(input) do
      String.split(input, "\n", trim: true)
      |> Enum.map(fn line ->
        [s, r] = String.split(line, " ")
        springs = process_springs(s)
        u = Enum.filter(springs, &(&1 == "?")) |> Enum.count()
        d = Enum.filter(springs, &(&1 == "#")) |> Enum.count()
        records = String.split(r, ",") |> Enum.map(&String.to_integer/1)
        total_d = Enum.sum(records)

        %Spring{
          springs: springs,
          records: records,
          unknown: u,
          dmg: d,
          dmg_t: total_d
        }
      end)
    end

    def unfold([], _), do: []

    def unfold([sp = %Spring{} | tail], n) do
      [unfold(sp, n) | unfold(tail, n)]
    end

    def unfold(sp = %Spring{}, n) do
      %Spring{
        springs: Enum.map_join(1..n, "?", fn _ -> sp.springs end) |> String.graphemes(),
        records: Enum.flat_map(1..n, fn _ -> sp.records end),
        unknown: n * sp.unknown + n - 1,
        dmg: n * sp.dmg,
        dmg_t: n * sp.dmg_t
      }
    end

    def guess_operational(sp = %Spring{springs: ["?" | stail]}) do
      Map.replace!(sp, :springs, ["." | stail]) |> Map.update!(:unknown, &(&1 - 1))
    end

    def guess_damaged(sp = %Spring{springs: ["?" | stail]}) do
      Map.replace!(sp, :springs, ["#" | stail])
      |> Map.update!(:unknown, &(&1 - 1))
      |> Map.update!(:dmg, &(&1 + 1))
    end

    def combine_list(springs) when is_list(springs) do
      combine_list(springs, 0)
    end

    def combine_list([], _), do: []

    def combine_list([sp = %Spring{} | tail], c) do
      [combine(sp) | combine_list(tail, c + 1)]
    end

    def combine(sp = %Spring{springs: s, records: r}) do
      if cache = Process.get({s, r}) do
        cache
      else
        cache = combiner(sp)
        Process.put({s, r}, cache)
        cache
      end
    end

    def combiner(%Spring{unknown: u, dmg: d, dmg_t: dt}) when u + d < dt, do: 0
    def combiner(%Spring{dmg: d, dmg_t: dt}) when d > dt, do: 0
    def combiner(%Spring{springs: ["#" | _], records: []}), do: 0
    def combiner(%Spring{springs: [], records: [_]}), do: 0

    def combiner(sp = %Spring{springs: ["#" | stail], records: [1]}) do
      combine(
        Map.replace!(sp, :records, [])
        |> Map.replace!(:springs, stail)
      )
    end

    def combiner(sp = %Spring{springs: ["#", "." | stail], records: [1 | rtail]}) do
      combine(
        Map.replace!(sp, :records, rtail)
        |> Map.replace!(:springs, stail)
      )
    end

    def combiner(sp = %Spring{springs: ["#", "?" | stail], records: [1 | rtail]}) do
      combine(
        Map.replace!(sp, :records, rtail)
        |> Map.replace!(:springs, stail)
        |> Map.update!(:unknown, &(&1 - 1))
      )
    end

    def combiner(%Spring{springs: ["#", "#" | _], records: [1 | _]}), do: 0

    def combiner(sp = %Spring{springs: ["#" | _], records: [_ | _]}) do
      combine_chain(sp)
    end

    def combiner(%Spring{unknown: 0, dmg: d, dmg_t: d}), do: 1

    def combiner(sp = %Spring{springs: ["?" | _]}) do
      combine(guess_operational(sp)) + combine(guess_damaged(sp))
    end

    def combiner(sp = %Spring{springs: ["." | stail]}),
      do: combine(Map.replace(sp, :springs, stail))

    def combine_chain(sp = %Spring{springs: [c | stail], records: [r | rtail]}) do
      case {c, r} do
        {"#", 1} ->
          combine(sp)

        {"#", _} ->
          combine_chain(
            Map.replace!(sp, :records, [r - 1 | rtail])
            |> Map.replace!(:springs, stail)
          )

        {"?", 1} ->
          combine(guess_damaged(sp))

        {"?", _} ->
          combine_chain(guess_damaged(sp))

        {".", _} ->
          0
      end
    end
  end

  def solve_pt1(input_data, _args) do
    Spring.parse(input_data) |> Spring.combine_list() |> Enum.sum()
  end

  def solve_pt2(input_data, _args) do
    Spring.parse(input_data)
    |> Spring.unfold(5)
    |> Spring.combine_list()
    |> Enum.sum()
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
