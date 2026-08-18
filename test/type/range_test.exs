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

  describe "bound constraints" do
    defp init!(constraints), do: Ash.Type.init(Ash.Type.Range, constraints) |> elem(1)

    defp apply!(range, constraints),
      do: Ash.Type.apply_constraints(Ash.Type.Range, range, constraints)

    test "a required bound must be there" do
      constraints = init!(inner_type: :integer, lower: [required?: true])

      assert {:error, _} = apply!(%Range{lower: nil, upper: 5, bounds: :"()"}, constraints)
      assert {:ok, %Range{lower: 1}} = apply!(%Range{lower: 1, upper: 5}, constraints)
    end

    test "each end is required independently" do
      constraints = init!(inner_type: :integer, upper: [required?: true])

      assert {:error, _} = apply!(%Range{lower: 1, upper: nil}, constraints)
      assert {:ok, _} = apply!(%Range{lower: nil, upper: 5, bounds: :"()"}, constraints)
    end

    test "a bound must carry the inclusivity asked for" do
      constraints = init!(inner_type: :datetime, upper: [inclusive?: false])

      assert {:ok, _} = apply!(%Range{lower: @lower, upper: @upper, bounds: :"[)"}, constraints)

      assert {:error, _} =
               apply!(%Range{lower: @lower, upper: @upper, bounds: :"[]"}, constraints)
    end

    test "inclusivity is not checked on an end that is not there" do
      constraints = init!(inner_type: :datetime, lower: [inclusive?: true])

      assert {:ok, _} = apply!(%Range{lower: nil, upper: @upper, bounds: :"()"}, constraints)
    end

    test "a discrete range canonicalized to an exclusive lower still satisfies inclusive?" do
      # `(,5]` canonicalizes to `(,6)`, so the lower is exclusive only because it is absent.
      constraints = init!(inner_type: :integer, lower: [inclusive?: true])

      {:ok, range} =
        Ash.Type.cast_input(
          Ash.Type.Range,
          %Range{lower: nil, upper: 5, bounds: :"(]"},
          constraints
        )

      assert {:ok, _} = apply!(range, constraints)
    end

    test "unconstrained by default" do
      constraints = init!(inner_type: :integer)

      assert {:ok, _} = apply!(%Range{lower: nil, upper: nil, bounds: :"()"}, constraints)
    end

    test "a period is the three constraints together" do
      constraints =
        init!(
          inner_type: :datetime,
          inner_constraints: [precision: :microsecond],
          lower: [required?: true, inclusive?: true],
          upper: [inclusive?: false]
        )

      assert {:ok, _} = apply!(%Range{lower: @lower, upper: nil, bounds: :"[)"}, constraints)
      assert {:error, _} = apply!(%Range{lower: nil, upper: @upper, bounds: :"()"}, constraints)

      assert {:error, _} =
               apply!(%Range{lower: @lower, upper: @upper, bounds: :"[]"}, constraints)

      assert {:error, _} = apply!(Range.empty(), constraints)
    end
  end

  describe "Allen relations" do
    defp ar(lower, upper, bounds \\ :"[)"),
      do: %Range{lower: lower, upper: upper, bounds: bounds}

    defp pairs do
      [
        precedes: {ar(1, 3), ar(5, 7)},
        meets: {ar(1, 5), ar(5, 7)},
        overlaps: {ar(1, 5), ar(3, 7)},
        finished_by: {ar(1, 7), ar(3, 7)},
        contains: {ar(1, 9), ar(3, 7)},
        starts: {ar(1, 5), ar(1, 7)},
        equals: {ar(1, 5), ar(1, 5)},
        started_by: {ar(1, 7), ar(1, 5)},
        during: {ar(3, 5), ar(1, 7)},
        finishes: {ar(3, 7), ar(1, 7)},
        overlapped_by: {ar(3, 7), ar(1, 5)},
        met_by: {ar(5, 7), ar(1, 5)},
        preceded_by: {ar(5, 7), ar(1, 3)}
      ]
    end

    test "each of the thirteen" do
      for {expected, {left, right}} <- pairs() do
        assert Range.relation(left, right) == expected
      end
    end

    test "the pairs cover every relation, in canonical order" do
      assert Keyword.keys(pairs()) == Range.relations()
    end

    test "converses mirror: swapping the operands walks the order inward" do
      for {expected, {left, right}} <- pairs() do
        converse =
          Enum.at(Range.relations(), 12 - Enum.find_index(Range.relations(), &(&1 == expected)))

        assert Range.relation(right, left) == converse
      end
    end

    test "meets needs exactly one side to include the point" do
      assert Range.relation(ar(1, 5, :"[)"), ar(5, 9, :"[)")) == :meets
      assert Range.relation(ar(1, 5, :"[]"), ar(5, 9, :"[)")) == :overlaps
      assert Range.relation(ar(1, 5, :"[)"), ar(5, 9, :"()")) == :precedes
    end

    test "an unbounded end reaches past every bound" do
      assert Range.relation(ar(nil, nil, :"()"), ar(1, 5)) == :contains
      assert Range.relation(ar(1, nil), ar(1, 5)) == :started_by
      assert Range.relation(ar(nil, 5, :"()"), ar(1, 5)) == :finished_by
    end

    test "an empty range has no relation" do
      assert Range.relation(Range.empty(), ar(1, 5)) == nil
      assert Range.relation(ar(1, 5), Range.empty()) == nil
      assert Range.relation(Range.empty(), Range.empty()) == nil
    end
  end

  describe "adjacent?/2" do
    # Cross-checked against Postgres 19: `-|-` on numrange and int4range agrees case
    # for case, canonical form included.
    defp adj(lower, upper, bounds), do: %Range{lower: lower, upper: upper, bounds: bounds}

    test "one ends where the other begins, with exactly one side including the seam" do
      assert Range.adjacent?(adj(1, 5, :"[)"), adj(5, 9, :"[)"))
      assert Range.adjacent?(adj(1, 5, :"()"), adj(5, 9, :"[)"))
      assert Range.adjacent?(adj(1, 5, :"[)"), adj(5, 9, :"[]"))
    end

    test "sharing the seam is overlap, and excluding it from both leaves a gap" do
      refute Range.adjacent?(adj(1, 5, :"[]"), adj(5, 9, :"[)"))
      refute Range.adjacent?(adj(1, 5, :"()"), adj(5, 9, :"()"))
      refute Range.adjacent?(adj(1, 5, :"[)"), adj(6, 9, :"[)"))
    end

    test "is symmetric, where Allen's meets is directional" do
      left = adj(1, 5, :"[)")
      right = adj(5, 9, :"[)")

      assert Range.relation(left, right) == :meets
      assert Range.relation(right, left) == :met_by
      assert Range.adjacent?(left, right)
      assert Range.adjacent?(right, left)
    end

    test "an empty range is adjacent to nothing, not even itself" do
      refute Range.adjacent?(Range.empty(), adj(1, 9, :"[)"))
      refute Range.adjacent?(adj(1, 9, :"[)"), Range.empty())
      refute Range.adjacent?(Range.empty(), Range.empty())
    end

    test "a discrete inner type answers on the canonical form" do
      {:ok, constraints} = Ash.Type.init(Ash.Type.Range, inner_type: :integer)
      {:ok, left} = Ash.Type.cast_input(Ash.Type.Range, adj(1, 4, :"[]"), constraints)
      {:ok, right} = Ash.Type.cast_input(Ash.Type.Range, adj(5, 9, :"[)"), constraints)

      assert {left.lower, left.upper, left.bounds} == {1, 5, :"[)"}
      assert Range.adjacent?(left, right)
      refute Range.adjacent?(adj(1, 4, :"[]"), adj(5, 9, :"[)"))
    end

    test "adjacent ranges tile, covering everything between without overlapping" do
      tiles = [adj(1, 5, :"[)"), adj(5, 9, :"[)"), adj(9, 13, :"[)")]

      assert tiles
             |> Enum.chunk_every(2, 1, :discard)
             |> Enum.all?(fn [a, b] ->
               Range.adjacent?(a, b)
             end)
    end
  end

  describe "ordering" do
    # Expectations are Postgres's own answers, on `numrange` so canonicalization
    # doesn't rewrite the bounds first.
    defp r(lower, upper, bounds \\ :"[)"), do: %Range{lower: lower, upper: upper, bounds: bounds}

    test "orders by lower bound first" do
      assert Ash.Range.compare(r(1, 9), r(5, 9)) == :lt
      assert Ash.Range.compare(r(5, 9), r(1, 9)) == :gt
    end

    test "orders by upper bound where lowers are equal" do
      assert Ash.Range.compare(r(1, 3), r(1, 9)) == :lt
    end

    test "identical ranges are equal" do
      assert Ash.Range.compare(r(1, 9), r(1, 9)) == :eq
    end

    test "an inclusive lower starts before an exclusive one at the same value" do
      assert Ash.Range.compare(r(1, 5, :"[)"), r(1, 5, :"()")) == :lt
    end

    test "an exclusive upper ends before an inclusive one at the same value" do
      assert Ash.Range.compare(r(1, 5, :"[)"), r(1, 5, :"[]")) == :lt
    end

    test "an unbounded lower is minus infinity" do
      assert Ash.Range.compare(r(nil, 5, :"()"), r(1, 5)) == :lt
    end

    test "an unbounded upper is plus infinity" do
      assert Ash.Range.compare(r(1, nil), r(1, 5)) == :gt
    end

    test "the empty range sorts first" do
      assert Ash.Range.compare(Range.empty(), r(nil, nil, :"()")) == :lt
      assert Ash.Range.compare(Range.empty(), Range.empty()) == :eq
    end

    test "Comp uses the comparator rather than term order" do
      assert Comp.compare(r(1, 9, :"()"), r(1, 3, :"[)")) == :gt
    end

    test "sorting agrees with Postgres" do
      sorted =
        [r(1, 5), r(nil, nil, :"()"), r(1, 3), Range.empty(), r(5, 9)]
        |> Enum.sort(Ash.Range)

      assert [
               %Range{empty?: true},
               %Range{lower: nil, upper: nil},
               %Range{lower: 1, upper: 3},
               %Range{lower: 1, upper: 5},
               %Range{lower: 5, upper: 9}
             ] = sorted
    end
  end

  describe "discrete ranges" do
    # Every expectation here is what Postgres renders for the same range, e.g.
    # `'[1,5]'::int4range` is `[1,6)` and `'(1,2)'::int4range` is `empty`.
    setup do
      {:ok, int} = Ash.Type.init(Ash.Type.Range, inner_type: :integer)
      {:ok, date} = Ash.Type.init(Ash.Type.Range, inner_type: :date)
      %{int: int, date: date}
    end

    defp cast!(range, constraints) do
      {:ok, cast} = Ash.Type.cast_input(Ash.Type.Range, range, constraints)
      cast
    end

    test "an inclusive upper moves on to the next value", %{int: int} do
      assert %Range{lower: 1, upper: 6, bounds: :"[)"} =
               cast!(%Range{lower: 1, upper: 5, bounds: :"[]"}, int)
    end

    test "an exclusive lower moves on to the next value", %{int: int} do
      assert %Range{lower: 2, upper: 5, bounds: :"[)"} =
               cast!(%Range{lower: 1, upper: 5, bounds: :"()"}, int)
    end

    test "both bounds move where both are non-canonical", %{int: int} do
      assert %Range{lower: 2, upper: 6, bounds: :"[)"} =
               cast!(%Range{lower: 1, upper: 5, bounds: :"(]"}, int)
    end

    test "a range already in canonical form is unchanged", %{int: int} do
      assert %Range{lower: 1, upper: 5, bounds: :"[)"} =
               cast!(%Range{lower: 1, upper: 5, bounds: :"[)"}, int)
    end

    test "spellings of the same set cast to one value", %{int: int} do
      assert cast!(%Range{lower: 1, upper: 5, bounds: :"[]"}, int) ==
               cast!(%Range{lower: 1, upper: 6, bounds: :"[)"}, int)
    end

    test "a single point keeps its point", %{int: int} do
      assert %Range{lower: 5, upper: 6, bounds: :"[)"} =
               cast!(%Range{lower: 5, upper: 5, bounds: :"[]"}, int)
    end

    test "bounds with no value between them are empty", %{int: int} do
      assert %Range{empty?: true} = cast!(%Range{lower: 1, upper: 2, bounds: :"()"}, int)
    end

    test "an unbounded end is exclusive", %{int: int} do
      assert %Range{lower: nil, upper: 6, bounds: :"()"} =
               cast!(%Range{lower: nil, upper: 5, bounds: :"[]"}, int)

      assert %Range{lower: 1, upper: nil, bounds: :"[)"} =
               cast!(%Range{lower: 1, upper: nil, bounds: :"[]"}, int)
    end

    test "dates canonicalise by day", %{date: date} do
      assert %Range{lower: ~D[2026-01-01], upper: ~D[2026-01-09], bounds: :"[)"} =
               cast!(%Range{lower: ~D[2026-01-01], upper: ~D[2026-01-08], bounds: :"[]"}, date)
    end

    test "a lower bound above its upper is left for apply_constraints to reject", %{int: int} do
      range = cast!(%Range{lower: 5, upper: 1, bounds: :"[]"}, int)

      assert %Range{lower: 5, upper: 1, bounds: :"[]"} = range
      assert {:error, _} = Ash.Type.apply_constraints(Ash.Type.Range, range, int)
    end

    test "a continuous inner type is left as written" do
      assert %Range{lower: @lower, upper: @upper, bounds: :"[]"} =
               cast!(%Range{lower: @lower, upper: @upper, bounds: :"[]"}, @constraints)
    end

    test "canonicalisation applies on the way out of storage too", %{int: int} do
      assert {:ok, %Range{lower: 1, upper: 6, bounds: :"[)"}} =
               Ash.Type.cast_stored(
                 Ash.Type.Range,
                 %Range{lower: 1, upper: 5, bounds: :"[]"},
                 int
               )
    end

    test "apply_constraints canonicalises a value that skipped casting", %{int: int} do
      assert {:ok, %Range{lower: 1, upper: 6, bounds: :"[)"}} =
               Ash.Type.apply_constraints(
                 Ash.Type.Range,
                 %Range{lower: 1, upper: 5, bounds: :"[]"},
                 int
               )
    end
  end

  describe "empty ranges" do
    test "a range admitting no points casts to the empty range" do
      assert {:ok, %Range{empty?: true, lower: nil, upper: nil}} =
               Ash.Type.cast_input(
                 Ash.Type.Range,
                 %Range{lower: @lower, upper: @lower},
                 @constraints
               )
    end

    test "every empty range casts to the same value" do
      {:ok, at_lower} =
        Ash.Type.cast_input(Ash.Type.Range, %Range{lower: @lower, upper: @lower}, @constraints)

      {:ok, at_upper} =
        Ash.Type.cast_input(Ash.Type.Range, %Range{lower: @upper, upper: @upper}, @constraints)

      assert at_lower == at_upper
      assert at_lower == Range.empty()
    end

    test "a range with both bounds inclusive is the single point, not empty" do
      assert {:ok, %Range{empty?: false, lower: @lower, upper: @lower}} =
               Ash.Type.cast_input(
                 Ash.Type.Range,
                 %Range{lower: @lower, upper: @lower, bounds: :"[]"},
                 @constraints
               )
    end

    test "an unbounded range is not empty" do
      assert {:ok, %Range{empty?: false}} =
               Ash.Type.cast_input(Ash.Type.Range, %Range{lower: nil, upper: nil}, @constraints)
    end

    test "the empty range survives a round trip through a data layer" do
      {:ok, range} =
        Ash.Type.cast_input(Ash.Type.Range, %Range{lower: @lower, upper: @lower}, @constraints)

      {:ok, native} = Ash.Type.dump_to_native(Ash.Type.Range, range, @constraints)

      assert {:ok, %Range{empty?: true}} =
               Ash.Type.cast_stored(Ash.Type.Range, native, @constraints)
    end

    test "a lower bound above the upper is rejected, not emptied" do
      {:ok, range} =
        Ash.Type.cast_input(Ash.Type.Range, %Range{lower: @upper, upper: @lower}, @constraints)

      refute range.empty?
      assert {:error, _} = Ash.Type.apply_constraints(Ash.Type.Range, range, @constraints)
    end

    test "an empty range is refused unless the attribute allows it" do
      assert {:error, _} =
               Ash.Type.apply_constraints(Ash.Type.Range, Range.empty(), @constraints)
    end

    test "allow_empty? keeps the empty range" do
      {:ok, constraints} =
        Ash.Type.init(Ash.Type.Range, inner_type: :integer, allow_empty?: true)

      assert {:ok, %Range{empty?: true}} =
               Ash.Type.apply_constraints(Ash.Type.Range, Range.empty(), constraints)
    end

    test "empty?/1 reports emptiness whether canonical or implied by the bounds" do
      assert Range.empty?(Range.empty())
      assert Range.empty?(%Range{lower: 5, upper: 5, bounds: :"[)"})
      assert Range.empty?(%Range{lower: 6, upper: 5, bounds: :"[]"})
      refute Range.empty?(%Range{lower: 5, upper: 5, bounds: :"[]"})
      refute Range.empty?(%Range{lower: 5, upper: 6, bounds: :"[)"})
      refute Range.empty?(%Range{lower: nil, upper: nil})
    end
  end
end
