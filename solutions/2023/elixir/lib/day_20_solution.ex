defmodule Day20 do
  @moduledoc """
  Advent Of Code 2023 - day 20
  https://adventofcode.com/2023/day/20

    --- Day 20: Pulse Propagation ---
  With your help, the Elves manage to find the right parts and fix all of the machines. Now, they
  just need to send the command to boot up the machines and get the sand flowing again.

  The machines are far apart and wired together with long cables. The cables don't connect to the
  machines directly, but rather to communication modules attached to the machines that perform
  various initialization tasks and also act as communication relays.

  Modules communicate using pulses. Each pulse is either a high pulse or a low pulse. When a module
  sends a pulse, it sends that type of pulse to each module in its list of destination modules.

  There are several different types of modules:

  Flip-flop modules (prefix %) are either on or off; they are initially off. If a flip-flop module
  receives a high pulse, it is ignored and nothing happens. However, if a flip-flop module receives
  a low pulse, it flips between on and off. If it was off, it turns on and sends a high pulse. If it
  was on, it turns off and sends a low pulse.

  Conjunction modules (prefix &) remember the type of the most recent pulse received from each of
  their connected input modules; they initially default to remembering a low pulse for each input.
  When a pulse is received, the conjunction module first updates its memory for that input. Then, if
  it remembers high pulses for all inputs, it sends a low pulse; otherwise, it sends a high pulse.

  There is a single broadcast module (named broadcaster). When it receives a pulse, it sends the
  same pulse to all of its destination modules.

  Here at Desert Machine Headquarters, there is a module with a single button on it called, aptly,
  the button module. When you push the button, a single low pulse is sent directly to the
  broadcaster module.

  After pushing the button, you must wait until all pulses have been delivered and fully handled
  before pushing it again. Never push the button if modules are still processing pulses.

  Pulses are always processed in the order they are sent. So, if a pulse is sent to modules a, b,
  and c, and then module a processes its pulse and sends more pulses, the pulses sent to modules b
  and c would have to be handled first.

  The module configuration (your puzzle input) lists each module. The name of the module is preceded
  by a symbol identifying its type, if any. The name is then followed by an arrow and a list of its
  destination modules. For example:
  broadcaster -> a, b, c
  %a -> b
  %b -> c
  %c -> inv
  &inv -> a


  In this module configuration, the broadcaster has three destination modules named a, b, and c.
  Each of these modules is a flip-flop module (as indicated by the % prefix). a outputs to b which
  outputs to c which outputs to another module named inv. inv is a conjunction module (as indicated
  by the & prefix) which, because it has only one input, acts like an inverter (it sends the
  opposite of the pulse type it receives); it outputs to a.

  By pushing the button once, the following pulses are sent:
  button -low-> broadcaster
  broadcaster -low-> a
  broadcaster -low-> b
  broadcaster -low-> c
  a -high-> b
  b -high-> c
  c -high-> inv
  inv -low-> a
  a -low-> b
  b -low-> c
  c -low-> inv
  inv -high-> a


  After this sequence, the flip-flop modules all end up off, so pushing the button again repeats the
  same sequence.

  Here's a more interesting example:
  broadcaster -> a
  %a -> inv, con
  &inv -> b
  %b -> con
  &con -> output


  This module configuration includes the broadcaster, two flip-flops (named a and b), a single-input
  conjunction module (inv), a multi-input conjunction module (con), and an untyped module named
  output (for testing purposes). The multi-input conjunction module con watches the two flip-flop
  modules and, if they're both on, sends a low pulse to the output module.

  Here's what happens if you push the button once:
  button -low-> broadcaster
  broadcaster -low-> a
  a -high-> inv
  a -high-> con
  inv -low-> b
  con -high-> output
  b -high-> con
  con -low-> output


  Both flip-flops turn on and a low pulse is sent to output! However, now that both flip-flops are
  on and con remembers a high pulse from each of its two inputs, pushing the button a second time
  does something different:
  button -low-> broadcaster
  broadcaster -low-> a
  a -low-> inv
  a -low-> con
  inv -high-> b
  con -high-> output


  Flip-flop a turns off! Now, con remembers a low pulse from module a, and so it sends only a high
  pulse to output.

  Push the button a third time:
  button -low-> broadcaster
  broadcaster -low-> a
  a -high-> inv
  a -high-> con
  inv -low-> b
  con -low-> output
  b -low-> con
  con -high-> output


  This time, flip-flop a turns on, then flip-flop b turns off. However, before b can turn off, the
  pulse sent to con is handled first, so it briefly remembers all high pulses for its inputs and
  sends a low pulse to output. After that, flip-flop b turns off, which causes con to update its
  state and send a high pulse to output.

  Finally, with a on and b off, push the button a fourth time:
  button -low-> broadcaster
  broadcaster -low-> a
  a -low-> inv
  a -low-> con
  inv -high-> b
  con -high-> output


  This completes the cycle: a turns off, causing con to remember only low pulses and restoring all
  modules to their original states.

  To get the cables warmed up, the Elves have pushed the button 1000 times. How many pulses got sent
  as a result (including the pulses sent by the button itself)?

  In the first example, the same thing happens every time the button is pushed: 8 low pulses and 4
  high pulses are sent. So, after pushing the button 1000 times, 8000 low pulses and 4000 high
  pulses are sent. Multiplying these together gives 32000000.

  In the second example, after pushing the button 1000 times, 4250 low pulses and 2750 high pulses
  are sent. Multiplying these together gives 11687500.

  Consult your module configuration; determine the number of low pulses and high pulses that would
  be sent after pushing the button 1000 times, waiting for all pulses to be fully handled after each
  push of the button. What do you get if you multiply the total number of low pulses sent by the
  total number of high pulses sent?

  --- Part Two ---
  The final machine responsible for moving the sand down to Island Island has a module attached
  named rx. The machine turns on when a single low pulse is sent to rx.

  Reset all modules to their default states. Waiting for all pulses to be fully handled after each
  button press, what is the fewest number of button presses required to deliver a single low pulse
  to the module named rx?
  """
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
      # Enum.filter(
      #   br,
      #   fn
      #     {k, %Conjunction{}} -> true
      #     _ -> false
      #   end
      # )
      # |> Enum.map(fn {k, v} -> {k, v.mem} end)
      # |> IO.inspect()

      {br, _, history} = pulse(br, pulses, %{low: 0, high: 0}, [])

      # presses |> IO.inspect()
      # history |> IO.inspect()

      if Enum.find(history, fn %Pulse{to: to, value: value} ->
           to == needle.to && value == needle.value
         end) do
        {presses, Enum.reverse(history)}
      else
        find(br, pulses, needle, presses + 1)
      end
    end
  end

  @doc """
      iex> Day20.solve_pt1(\"""
      ...>broadcaster -> a, b, c
      ...>%a -> b
      ...>%b -> c
      ...>%c -> inv
      ...>&inv -> a
      ...>\""")
      32000000

      iex> Day20.solve_pt1(\"""
      ...>broadcaster -> a
      ...>%a -> inv, con
      ...>&inv -> b
      ...>%b -> con
      ...>&con -> output
      ...>\""")
      11687500
  """
  def solve_pt1(input) do
    br = Broadcaster.parse(input)

    repeats = 1000

    {_, counter, _} =
      Broadcaster.pulse(br, [%Pulse{from: :button, to: :broadcaster, value: :low}], repeats)

    counter.low * counter.high
  end

  @doc """
      iex> Day20.solve_pt2(\"""
      ...>broadcaster -> a, b, c
      ...>%a -> b
      ...>%b -> c
      ...>%c -> inv
      ...>&inv -> a
      ...>\""")
      32000000
  """
  def solve_pt2(input) do
    br = Broadcaster.parse(input)

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

  def main do
    ans1 =
      File.read!("../data/day_20_input.txt")
      |> Day20.solve_pt1()

    IO.puts("Part one: #{ans1}")

    ans2 =
      File.read!("../data/day_20_input.txt")
      |> Day20.solve_pt2()

    IO.puts("Part two: #{ans2}")
  end
end
