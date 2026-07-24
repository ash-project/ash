# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.InTest.Version do
  @moduledoc false
  defstruct [:number]
end

import Ash.Type.Comparable

# Stands in for the comparability any application can add for its own types with
# `Ash.Type.Comparable.defcomparable/3`. `In` cannot know about it ahead of time,
# which is why set membership alone can never decide a miss.
defcomparable left :: Ash.Query.Operator.InTest.Version, right :: BitString do
  Comp.compare(left.number, right)
end

defmodule Ash.Query.Operator.InTest do
  use ExUnit.Case

  import Ash.Expr

  alias Ash.Query.Operator.Eq
  alias Ash.Query.Operator.In
  alias Ash.Query.Operator.InTest.Version
  alias Ash.Test.Domain, as: Domain

  describe "compare/2" do
    test "returns :mutually_inclusive for equal left and right values" do
      left = %In{left: 1, right: MapSet.new([2, 3])}
      right = %In{left: 1, right: MapSet.new([2, 3])}

      assert In.compare(left, right) == :mutually_inclusive
    end

    test "returns :mutually_exclusive for different right values" do
      left = %In{left: 1, right: MapSet.new([2, 3])}
      right = %In{left: 1, right: MapSet.new([4, 5])}

      assert In.compare(left, right) == :mutually_exclusive
    end

    test "returns :left_includes_right for Eq in In" do
      left = %In{left: 1, right: MapSet.new([2, 3])}
      right = %Eq{left: 1, right: 2}

      assert In.compare(left, right) == :left_includes_right
    end
  end

  describe "evaluate/1" do
    test "a member of the set matches" do
      assert In.evaluate(%In{left: "b", right: MapSet.new(["a", "b", "c"])}) == {:known, true}
    end

    test "a value absent from the set does not match" do
      assert In.evaluate(%In{left: "z", right: MapSet.new(["a", "b", "c"])}) == {:known, false}
    end

    test "an empty set matches nothing" do
      assert In.evaluate(%In{left: "a", right: MapSet.new([])}) == {:known, false}
    end

    test "a nil operand is unknown rather than false" do
      assert In.evaluate(%In{left: nil, right: MapSet.new(["a"])}) == {:known, nil}
      assert In.evaluate(%In{left: "a", right: nil}) == {:known, nil}
    end

    test "a list on the right is supported alongside a set" do
      assert In.evaluate(%In{left: "b", right: ["a", "b"]}) == {:known, true}
      assert In.evaluate(%In{left: "z", right: ["a", "b"]}) == {:known, false}
    end
  end

  # `Comp.equal?/2` is semantic, not structural. Each of these values is absent
  # from the set under term equality but equal to a member under `Comp`, so each
  # one only matches if the set miss falls back to comparing the members.
  describe "evaluate/1 with semantically equal but structurally distinct members" do
    test "integers and floats" do
      assert In.evaluate(%In{left: 1, right: MapSet.new([1.0])}) == {:known, true}
      assert In.evaluate(%In{left: 1.0, right: MapSet.new([1])}) == {:known, true}
    end

    test "decimals against integers and strings" do
      assert In.evaluate(%In{left: Decimal.new(1), right: MapSet.new([1])}) == {:known, true}
      assert In.evaluate(%In{left: 1, right: MapSet.new([Decimal.new(1)])}) == {:known, true}
      assert In.evaluate(%In{left: Decimal.new(1), right: MapSet.new(["1"])}) == {:known, true}
    end

    test "decimals differing only in trailing zeroes" do
      assert In.evaluate(%In{left: Decimal.new("1.0"), right: MapSet.new([Decimal.new("1.00")])}) ==
               {:known, true}
    end

    test "case insensitive strings against binaries" do
      assert In.evaluate(%In{left: Ash.CiString.new("FOO"), right: MapSet.new(["foo"])}) ==
               {:known, true}

      assert In.evaluate(%In{left: "FOO", right: MapSet.new([Ash.CiString.new("foo")])}) ==
               {:known, true}
    end

    test "atoms against binaries" do
      assert In.evaluate(%In{left: :foo, right: MapSet.new(["foo"])}) == {:known, true}
    end

    # Since OTP 27 `0.0` and `-0.0` are distinct terms under `===`, and so under
    # the term equality a `MapSet` is built on — `MapSet.new([0.0, -0.0])` has two
    # members. They are equal under `==`, and so under `Comp`, which is what `in`
    # has always meant here.
    test "signed zeroes, which a set holds apart but Comp does not" do
      assert In.evaluate(%In{left: -0.0, right: MapSet.new([0.0])}) == {:known, true}
      assert In.evaluate(%In{left: 0.0, right: MapSet.new([-0.0])}) == {:known, true}
      assert In.evaluate(%In{left: -0.0, right: MapSet.new([0])}) == {:known, true}
    end

    test "signed zero decimals, which are distinct terms but compare equal" do
      positive = Decimal.new("0.0")
      negative = Decimal.new("-0.0")

      refute positive == negative
      assert Decimal.compare(positive, negative) == :eq

      assert In.evaluate(%In{left: negative, right: MapSet.new([positive])}) == {:known, true}
      assert In.evaluate(%In{left: positive, right: MapSet.new([negative])}) == {:known, true}
      assert In.evaluate(%In{left: negative, right: MapSet.new([0])}) == {:known, true}
    end

    test "types made comparable by the application" do
      assert In.evaluate(%In{left: %Version{number: "1.0"}, right: MapSet.new(["1.0"])}) ==
               {:known, true}

      assert In.evaluate(%In{left: %Version{number: "1.0"}, right: MapSet.new(["2.0"])}) ==
               {:known, false}
    end
  end

  describe "filter_matches/3" do
    defmodule Post do
      @moduledoc false
      use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

      attributes do
        uuid_primary_key :id
        attribute :title, :string, public?: true
        attribute :score, :decimal, public?: true
      end

      actions do
        defaults [:read]
      end
    end

    defp matching_titles(posts, filter) do
      {:ok, matches} = Ash.Filter.Runtime.filter_matches(Domain, posts, filter)
      Enum.map(matches, & &1.title)
    end

    setup do
      %{posts: Enum.map(["a", "b", "c"], &struct(Post, title: &1, score: Decimal.new(1)))}
    end

    test "selects exactly the records whose value is in the list", %{posts: posts} do
      filter = Ash.Filter.parse!(Post, expr(title in ["a", "c"]))

      assert matching_titles(posts, filter) == ["a", "c"]
    end

    test "selects nothing when no record's value is in the list", %{posts: posts} do
      filter = Ash.Filter.parse!(Post, expr(title in ["x", "y"]))

      assert matching_titles(posts, filter) == []
    end

    test "selects records whose value is only semantically in the list", %{posts: posts} do
      filter = Ash.Filter.parse!(Post, expr(score in [1]))

      assert matching_titles(posts, filter) == ["a", "b", "c"]
    end
  end
end
