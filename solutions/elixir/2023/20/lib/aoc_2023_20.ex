defmodule Year2023Day20 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 20
  Elixir Solution

  Day 20: Pulse Propagation

  https://adventofcode.com/2023/day/20
  """
  import EsbFireplace

  defmodule LCM do
    def lcm(0, 0), do: 0
    def lcm(a, b), do: Kernel.trunc(a * b / Integer.gcd(a, b))
  end

  defmodule Pulse do
    defstruct [:from, :to, :value]
  end

  defmodule BroadcasterModule do
    defstruct [:dst, name: :broadcaster]
  end

  defmodule FlipFlop do
    defstruct [:name, :dst, state: :off]
  end

  defmodule Conjunction do
    defstruct [:name, :dst, :mem]
  end

  defmodule Module do
    def pulse(nil, _) do
      {nil, []}
    end

    def pulse(module = %BroadcasterModule{dst: dst}, %Pulse{value: value}) do
      {module, Enum.map(dst, fn d -> %Pulse{from: :broadcaster, to: d, value: value} end)}
    end

    def pulse(module = %FlipFlop{dst: dst}, %Pulse{to: name, value: value}) do
      case {value, module.state} do
        {:high, _} ->
          {module, []}

        {:low, :off} ->
          {Map.replace(module, :state, :on),
           Enum.map(dst, &%Pulse{from: name, to: &1, value: :high})}

        {:low, :on} ->
          {Map.replace(module, :state, :off),
           Enum.map(dst, &%Pulse{from: name, to: &1, value: :low})}
      end
    end

    def pulse(module = %Conjunction{dst: dst}, %Pulse{from: from, to: name, value: value}) do
      module = Map.update!(module, :mem, &Map.replace!(&1, from, value))
      v = if Enum.all?(module.mem, &(elem(&1, 1) == :high)), do: :low, else: :high
      {module, Enum.map(dst, &%Pulse{from: name, to: &1, value: v})}
    end
  end

  defmodule Broadcaster do
    def parse(input) do
      modules =
        String.split(input, "\n", trim: true)
        |> Enum.map(fn line ->
          [name, dst] = String.split(line, " -> ")
          dst = String.split(dst, ", ") |> Enum.map(&String.to_atom/1)

          module =
            if name == "broadcaster" do
              %BroadcasterModule{dst: dst}
            else
              case String.split_at(name, 1) do
                {"%", name} -> %FlipFlop{name: String.to_atom(name), dst: dst}
                {"&", name} -> %Conjunction{name: String.to_atom(name), dst: dst}
              end
            end

          {module.name, module}
        end)
        |> Map.new()

      Enum.reduce(modules, modules, fn {k0, m0}, acc ->
        case m0 do
          %Conjunction{} ->
            src =
              Enum.map(modules, fn {k1, m1} -> if k0 in m1.dst, do: k1, else: nil end)
              |> Enum.filter(&(&1 !== nil))

            Map.replace(
              acc,
              k0,
              Map.replace!(m0, :mem, Enum.map(src, fn s -> {s, :low} end) |> Map.new())
            )

          _ ->
            acc
        end
      end)
    end

    def pulse(br = %{}, pulses = [], counter, history) when is_list(pulses),
      do: {br, counter, history}

    def pulse(
          br = %{},
          pulses = [pulse = %Pulse{to: name, value: value} | tail],
          counter = %{},
          history
        )
        when is_list(pulses) do
      counter = Map.update!(counter, value, &(&1 + 1))
      history = [pulse | history]

      case Module.pulse(br[name], pulse) do
        {nil, []} -> pulse(br, tail, counter, history)
        {nm, np} -> pulse(Map.replace!(br, name, nm), tail ++ np, counter, history)
      end
    end

    def pulse(br = %{}, pulses, repeats)
        when is_list(pulses) and is_integer(repeats) do
      counter = %{low: 0, high: 0}
      history = []

      Enum.reduce(1..repeats, {br, counter, history}, fn _, {br, counter, history} ->
        pulse(br, pulses, counter, history)
      end)
    end

    def find(br = %{}, pulses, needle, presses \\ 1) do
      {br, _, history} = pulse(br, pulses, %{low: 0, high: 0}, [])

      if Enum.find(history, fn %Pulse{to: to, value: value} ->
           to == needle.to && value == needle.value
         end) do
        {presses, Enum.reverse(history)}
      else
        find(br, pulses, needle, presses + 1)
      end
    end
  end

  defp solve_pt1(input_data, _args) do
    br = Broadcaster.parse(input_data)

    repeats = 1000

    {_, counter, _} =
      Broadcaster.pulse(br, [%Pulse{from: :button, to: :broadcaster, value: :low}], repeats)

    counter.low * counter.high
  end

  defp solve_pt2(input_data, _args) do
    br = Broadcaster.parse(input_data)

    nodes = [:lf, :br, :rz, :fk]

    Enum.map(
      nodes,
      &(Broadcaster.find(
          br,
          [%Pulse{from: :button, to: :broadcaster, value: :low}],
          %Pulse{from: :any, to: &1, value: :low}
        )
        |> elem(0))
    )
    |> Enum.reduce(1, &LCM.lcm(&1, &2))
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
