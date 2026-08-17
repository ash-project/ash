# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.RangeTest do
  use ExUnit.Case, async: true

  alias Ash.Range

  {:ok, constraints} =
    Ash.Type.init(Ash.Type.Range,
      inner_type: :datetime,
      inner_constraints: [precision: :microsecond]
    )

  {:ok, date_constraints} = Ash.Type.init(Ash.Type.Range, inner_type: :date)
  @constraints constraints
  @date_constraints date_constraints

  @lower ~U[2026-01-01 00:00:00.000000Z]
  @upper ~U[2026-02-01 00:00:00.000000Z]

  test "the :range short name resolves to Ash.Type.Range" do
    assert Ash.Type.get_type(:range) == Ash.Type.Range
  end

  test "storage_type is the logical :range (data layer chooses the concrete type)" do
    assert Ash.Type.Range.storage_type(@constraints) == :range
    assert Ash.Type.Range.storage_type(@date_constraints) == :range
  end

  test "generator/1 produces ordered ranges of the inner type" do
    {:ok, int_constraints} =
      Ash.Type.init(Ash.Type.Range, inner_type: :integer, inner_constraints: [min: 0, max: 100])

    int_constraints
    |> Ash.Type.Range.generator()
    |> Enum.take(50)
    |> Enum.each(fn %Range{lower: lower, upper: upper, bounds: :"[)"} ->
      assert is_integer(lower) and lower in 0..100
      assert is_integer(upper) and upper in 0..100
      assert lower <= upper
    end)
  end

  test "cast_input from an Ash.Range struct casts the bounds via the inner type" do
    assert {:ok, %Range{lower: @lower, upper: @upper, bounds: :"[)"}} =
             Ash.Type.cast_input(
               Ash.Type.Range,
               %Range{lower: @lower, upper: @upper},
               @constraints
             )
  end

  test "cast_input from a {lower, upper} tuple defaults bounds to [)" do
    assert {:ok, %Range{lower: @lower, upper: @upper, bounds: :"[)"}} =
             Ash.Type.cast_input(Ash.Type.Range, {@lower, @upper}, @constraints)
  end

  test "a nil bound is an unbounded end" do
    assert {:ok, %Range{lower: @lower, upper: nil}} =
             Ash.Type.cast_input(Ash.Type.Range, %Range{lower: @lower, upper: nil}, @constraints)
  end

  test "round-trips through dump_to_native and cast_stored" do
    {:ok, range} =
      Ash.Type.cast_input(Ash.Type.Range, %Range{lower: @lower, upper: @upper}, @constraints)

    {:ok, native} = Ash.Type.dump_to_native(Ash.Type.Range, range, @constraints)
    assert %{lower: _, upper: _, bounds: :"[)"} = native

    assert {:ok, ^range} = Ash.Type.cast_stored(Ash.Type.Range, native, @constraints)
  end

  test "apply_constraints rejects a lower bound greater than the upper" do
    {:ok, range} =
      Ash.Type.cast_input(Ash.Type.Range, %Range{lower: @upper, upper: @lower}, @constraints)

    assert {:error, _} = Ash.Type.apply_constraints(Ash.Type.Range, range, @constraints)
  end
end
