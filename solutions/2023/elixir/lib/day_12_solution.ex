defmodule Day12 do
  @moduledoc """
  Advent Of Code 2023 - day 12
  https://adventofcode.com/2023/day/12

    --- Day 12: Hot Springs ---
  You finally reach the hot springs! You can see steam rising from secluded areas attached to the
  primary, ornate building.

  As you turn to enter, the researcher stops you. "Wait - I thought you were looking for the hot
  springs, weren't you?" You indicate that this definitely looks like hot springs to you.

  "Oh, sorry, common mistake! This is actually the onsen! The hot springs are next door."

  You look in the direction the researcher is pointing and suddenly notice the massive metal helixes
  towering overhead. "This way!"

  It only takes you a few more steps to reach the main gate of the massive fenced-off area
  containing the springs. You go through the gate and into a small administrative building.

  "Hello! What brings you to the hot springs today? Sorry they're not very hot right now; we're
  having a lava shortage at the moment." You ask about the missing machine parts for Desert Island.

  "Oh, all of Gear Island is currently offline! Nothing is being manufactured at the moment, not
  until we get more lava to heat our forges. And our springs. The springs aren't very springy unless
  they're hot!"

  "Say, could you go up and see why the lava stopped flowing? The springs are too cold for normal
  operation, but we should be able to find one springy enough to launch you up there!"

  There's just one problem - many of the springs have fallen into disrepair, so they're not actually
  sure which springs would even be safe to use! Worse yet, their condition records of which springs
  are damaged (your puzzle input) are also damaged! You'll need to help them repair the damaged
  records.

  In the giant field just outside, the springs are arranged into rows. For each row, the condition
  records show every spring and whether it is operational (.) or damaged (#). This is the part of
  the condition records that is itself damaged; for some springs, it is simply unknown (?) whether
  the spring is operational or damaged.

  However, the engineer that produced the condition records also duplicated some of this information
  in a different format! After the list of springs for a given row, the size of each contiguous
  group of damaged springs is listed in the order those groups appear in the row. This list always
  accounts for every damaged spring, and each number is the entire size of its contiguous group
  (that is, groups are always separated by at least one operational spring: #### would always be 4,
  never 2,2).

  So, condition records with no unknown spring conditions might look like this:
  #.#.### 1,1,3
  .#...#....###. 1,1,3
  .#.###.#.###### 1,3,1,6
  ####.#...#... 4,1,1
  #....######..#####. 1,6,5
  .###.##....# 3,2,1


  However, the condition records are partially damaged; some of the springs' conditions are actually
  unknown (?). For example:
  ???.### 1,1,3
  .??..??...?##. 1,1,3
  ?#?#?#?#?#?#?#? 1,3,1,6
  ????.#...#... 4,1,1
  ????.######..#####. 1,6,5
  ?###???????? 3,2,1


  Equipped with this information, it is your job to figure out how many different arrangements of
  operational and broken springs fit the given criteria in each row.

  In the first line (???.### 1,1,3), there is exactly one way separate groups of one, one, and three
  broken springs (in that order) can appear in that row: the first three unknown springs must be
  broken, then operational, then broken (#.#), making the whole row #.#.###.

  The second line is more interesting: .??..??...?##. 1,1,3 could be a total of four different
  arrangements. The last ? must always be broken (to satisfy the final contiguous group of three
  broken springs), and each ?? must hide exactly one of the two broken springs. (Neither ?? could be
  both broken springs or they would form a single contiguous group of two; if that were true, the
  numbers afterward would have been 2,3 instead.) Since each ?? can either be #. or .#, there are
  four possible arrangements of springs.

  The last line is actually consistent with ten different arrangements! Because the first number is
  3, the first and second ? must both be . (if either were #, the first number would have to be 4 or
  higher). However, the remaining run of unknown spring conditions have many different ways they
  could hold groups of two and one broken springs:
  ?###???????? 3,2,1
  .###.##.#...
  .###.##..#..
  .###.##...#.
  .###.##....#
  .###..##.#..
  .###..##..#.
  .###..##...#
  .###...##.#.
  .###...##..#
  .###....##.#


  In this example, the number of possible arrangements for each row is:

  ???.### 1,1,3 - 1 arrangement
  .??..??...?##. 1,1,3 - 4 arrangements
  ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
  ????.#...#... 4,1,1 - 1 arrangement
  ????.######..#####. 1,6,5 - 4 arrangements
  ?###???????? 3,2,1 - 10 arrangements


  Adding all of the possible arrangement counts together produces a total of 21 arrangements.

  For each row, count all of the different arrangements of operational and broken springs that meet
  the given criteria. What is the sum of those counts?

  {description_pt2}
  """
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

  @doc """
      iex> Day12.solve_pt1(\"""
      ...>???.### 1,1,3
      ...>.??..??...?##. 1,1,3
      ...>?#?#?#?#?#?#?#? 1,3,1,6
      ...>????.#...#... 4,1,1
      ...>????.######..#####. 1,6,5
      ...>?###???????? 3,2,1
      ...>\""")
      21
  """
  def solve_pt1(input) do
    Spring.parse(input) |> Spring.combine_list() |> Enum.sum()
  end

  @doc """
      iex> Day12.solve_pt2(\"""
      ...>???.### 1,1,3
      ...>.??..??...?##. 1,1,3
      ...>?#?#?#?#?#?#?#? 1,3,1,6
      ...>????.#...#... 4,1,1
      ...>????.######..#####. 1,6,5
      ...>?###???????? 3,2,1
      ...>\""")
      525152
  """
  def solve_pt2(input) do
    Spring.parse(input)
    |> Spring.unfold(5)
    |> Spring.combine_list()
    |> Enum.sum()
  end

  def main do
    ans1 =
      File.read!("../data/day_12_input.txt")
      |> Day12.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_12_input.txt")
      |> Day12.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
