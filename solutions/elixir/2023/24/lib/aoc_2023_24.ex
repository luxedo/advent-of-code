defmodule Year2023Day24 do
  @moduledoc """
  ElfScript Brigade

  Advent Of Code 2023 Day 24
  Elixir Solution

  Day 24: Never Tell Me The Odds

  https://adventofcode.com/2023/day/24
  """
  import EsbFireplace

  defmodule Fraction do
    defstruct [:num, :den]

    def new({num, den}) do
      simplify(%Fraction{num: num, den: den})
    end

    def add(%Fraction{num: anum, den: aden}, %Fraction{num: bnum, den: bden}) do
      simplify(%Fraction{num: anum * bden + bnum * aden, den: aden * bden})
    end

    def sub(%Fraction{num: anum, den: aden}, %Fraction{num: bnum, den: bden}) do
      simplify(%Fraction{num: anum * bden - bnum * aden, den: aden * bden})
    end

    def mul(%Fraction{num: anum, den: aden}, %Fraction{num: bnum, den: bden}) do
      simplify(%Fraction{num: anum * bnum, den: aden * bden})
    end

    def div(%Fraction{num: anum, den: aden}, %Fraction{num: bnum, den: bden}) do
      simplify(%Fraction{num: anum * bden, den: aden * bnum})
    end

    def to_float(%Fraction{num: n, den: d}), do: n / d
    def to_integer(%Fraction{num: n, den: d}), do: Kernel.div(n, d)

    defp simplify(%Fraction{num: 0}) do
      %Fraction{num: 0, den: 1}
    end

    defp simplify(%Fraction{num: num, den: den}) do
      d = Integer.gcd(num, den)
      normalize(%Fraction{num: Kernel.div(num, d), den: Kernel.div(den, d)})
    end

    defp normalize(%Fraction{num: n, den: d}) when d < 0, do: %Fraction{num: -n, den: -d}
    defp normalize(frac), do: frac
  end

  defmodule Hail do
    defstruct [:x, :y, :z, :vx, :vy, :vz]

    def from_string(input) do
      [[x, y, z], [vx, vy, vz]] =
        String.split(input, "@")
        |> Enum.map(fn p ->
          String.trim(p)
          |> String.split(",")
          |> Enum.map(fn n -> String.trim(n) |> String.to_integer() end)
        end)

      %Hail{x: x, y: y, z: z, vx: vx, vy: vy, vz: vz}
    end

    def parse(input) do
      String.split(input, "\n", trim: true) |> Enum.map(&Hail.from_string/1)
    end

    def paths_intersect(hails) when is_list(hails) do
      Enum.slice(hails, 0..-2//1)
      |> Enum.with_index()
      |> Enum.flat_map(fn {h1, i} ->
        Enum.slice(hails, (i + 1)..-1//1)
        |> paths_intersect(h1)
      end)
      |> Map.new()
    end

    def paths_intersect(hails, h1 = %Hail{}) when is_list(hails) do
      Enum.map(hails, fn h0 ->
        {{h0, h1}, paths_intersect(h0, h1)}
      end)
    end

    def paths_intersect(h0 = %Hail{}, h1 = %Hail{}) do
      case slope_xy(h0, h1) do
        {:skewed} ->
          {:skewed}

        {:intercepts, ts = {t0, t1}} ->
          if h0.z + h0.vz * t0 - (h1.z + h1.vz * t1) != 0 do
            {:skewed}
          else
            {:intercepts,
             {
               h0.x + h0.vx * t0,
               h0.y + h0.vy * t0,
               h0.z + h0.vz * t0
             }, ts}
          end
      end
    end

    defp determinant(%Hail{vx: vx0, vy: vy0}, %Hail{vx: vx1, vy: vy1}) do
      -vx0 * vy1 + vx1 * vy0
    end

    defp slope_xy(
           h0 = %Hail{x: x0, y: y0, vx: vx0, vy: vy0},
           h1 = %Hail{x: x1, y: y1, vx: vx1, vy: vy1}
         ) do
      case determinant(h0, h1) do
        0 ->
          {:skewed}

        det ->
          t0 = (-vy1 * (x1 - x0) + vx1 * (y1 - y0)) / det
          t1 = (-vy0 * (x1 - x0) + vx0 * (y1 - y0)) / det
          {:intercepts, {t0, t1}}
      end
    end

    def in_range(hails_map, xlow..xhigh, ylow..yhigh, zlow..zhigh) when is_map(hails_map) do
      Enum.filter(hails_map, fn {_, value} ->
        case value do
          {:skewed} ->
            false

          {:intercepts, {x, y, z}, {t0, t1}} ->
            t0 >= 0 && t1 >= 0 && x >= xlow && x <= xhigh && y >= ylow && y <= yhigh && z >= zlow &&
              z <= zhigh
        end
      end)
      |> Map.new()
    end

    def intercept_equation_xy(
          %Hail{x: x0, y: y0, vx: vx0, vy: vy0},
          %Hail{x: x1, y: y1, vx: vx1, vy: vy1}
        ) do
      crvx = vy1 - vy0
      crvy = -(vx1 - vx0)
      cx = -(y1 - y0)
      cy = x1 - x0
      c = x1 * vy1 - y1 * vx1 - (x0 * vy0 - y0 * vx0)
      [crvx, crvy, cx, cy, c]
    end

    def intercept_equation_xz(
          %Hail{x: x0, z: z0, vx: vx0, vz: vz0},
          %Hail{x: x1, z: z1, vx: vx1, vz: vz1}
        ) do
      crvx = vz1 - vz0
      crvz = -(vx1 - vx0)
      cx = -(z1 - z0)
      cz = x1 - x0
      c = x1 * vz1 - z1 * vx1 - (x0 * vz0 - z0 * vx0)
      [crvx, crvz, cx, cz, c]
    end

    def gauss_elimination([[a, b]]) do
      [[a, b]]
    end

    def gauss_elimination(matrix) do
      [head | tail] = List.update_at(matrix, 0, &normalize_row/1)
      tail_matrix = Enum.map(tail, &(echelon_row(&1, head) |> List.delete_at(0)))
      [head | gauss_elimination(tail_matrix) |> Enum.map(&[Fraction.new({0, 1}) | &1])]
    end

    defp normalize_row(row = [head | _]) do
      Enum.map(row, fn item -> Fraction.div(item, head) end)
    end

    defp echelon_row(row = [head | _], reference) do
      Enum.zip(row, reference)
      |> Enum.map(fn {item, iref} ->
        Fraction.sub(item, Fraction.mul(iref, head))
      end)
    end

    def calc_coefficient([%Fraction{num: 0, den: _} | tail], coefs),
      do: calc_coefficient(tail, coefs)

    def calc_coefficient([c0 = %Fraction{}, c1 = %Fraction{}], _),
      do: Fraction.div(c1, c0)

    def calc_coefficient([a | tail], coefs) do
      [c1 = %Fraction{} | unk] = Enum.reverse(tail)

      b =
        Fraction.sub(
          c1,
          Enum.reverse(unk)
          |> Enum.zip(coefs)
          |> Enum.reduce(Fraction.new({0, 1}), fn {a, b}, c ->
            Fraction.mul(a, b) |> Fraction.add(c)
          end)
        )

      calc_coefficient([a, b], coefs)
    end

    def find_coefficients([head = [0 | _] | tail]), do: find_coefficients(tail ++ [head])

    def find_coefficients(matrix) do
      matrix =
        Enum.map(matrix, fn row ->
          Enum.map(row, &Fraction.new({&1, 1}))
        end)
        |> gauss_elimination()

      Enum.reverse(matrix)
      |> Enum.reduce([], fn row, acc ->
        [calc_coefficient(row, acc) | acc]
      end)
      |> Enum.map(&Fraction.to_integer/1)
    end
  end

  def solve_pt1(input_data, args) do
    default_range = 200_000_000_000_000..400_000_000_000_000
    [zrange, yrange] = case args do
      [z0, zf, y0, yf] -> [
        String.to_integer(z0)..String.to_integer(zf),
        String.to_integer(y0)..String.to_integer(yf),
      ]
      _ -> [
        default_range,
        default_range
      ]
    end
    #IO.inspect(zrange)
    # IO.inspect(yrange)
    Hail.parse(input_data)
    |> Enum.map(&Map.merge(&1, %{z: 0, vz: 0}))
    |> Hail.paths_intersect()
    |> Hail.in_range(zrange, yrange, 0..1)
    |> Enum.count()
  end

  def solve_pt2(input_data, _args) do
    hails = Hail.parse(input_data)

    [ref | tail] = Enum.slice(hails, 0..4)

    Enum.map(tail, &Hail.intercept_equation_xy(ref, &1))

    [x0, y, vx0, _] =
      Enum.map(tail, &Hail.intercept_equation_xy(ref, &1))
      |> Hail.find_coefficients()

    [x1, z, vx1, _] =
      Enum.map(tail, &Hail.intercept_equation_xz(ref, &1))
      |> Hail.find_coefficients()

    if x0 != x1 || vx0 != vx1 do
      raise "Oh no! There's something very very wrong"
    end

    x0 + y + z
  end

  def start do
    # 🎅🎄❄️☃️🎁🦌
    # Bright christmas lights HERE
    v1_run(&solve_pt1/2, &solve_pt2/2)
  end
end
