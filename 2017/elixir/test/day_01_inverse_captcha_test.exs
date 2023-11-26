defmodule Day01TestPt1 do
  use ExUnit.Case
  doctest Day01

  test "Test example 1 pt1" do
    assert Day01.solve_pt1("1122") == 3
  end

  test "Test example 2 pt1" do
    assert Day01.solve_pt1("1111") == 4
  end

  test "Test example 3 pt1" do
    assert Day01.solve_pt1("1234") == 0
  end

  test "Test example 4 pt1" do
    assert Day01.solve_pt1("91212129") == 9
  end
end

defmodule Day01TestPt2 do
  use ExUnit.Case
  doctest Day01

  test "Test example 1 pt2" do
    assert Day01.solve_pt2("1212") == 6
  end

  test "Test example 2 pt2" do
    assert Day01.solve_pt2("1221") == 0
  end

  test "Test example 3 pt2" do
    assert Day01.solve_pt2("123425") == 4
  end

  test "Test example 4 pt2" do
    assert Day01.solve_pt2("123123") == 12
  end

  test "Test example 5 pt2" do
    assert Day01.solve_pt2("12131415") == 4
  end
end
