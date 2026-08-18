# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.CompMemoizeTest do
  @moduledoc false
  # Not async: `memoize_dispatch/1` is VM-global.
  use ExUnit.Case, async: false

  setup do
    on_exit(fn -> Comp.memoize_dispatch(false) end)
    Comp.memoize_dispatch(false)
  end

  defp cached_pairs do
    Enum.count(:persistent_term.get(), fn {key, _} ->
      match?({Comp, :dispatch, _, _}, key)
    end)
  end

  test "nothing is memoized while it is turned off" do
    assert Comp.equal?(1, 1)
    assert Comp.equal?(Decimal.new(1), 1)
    assert cached_pairs() == 0
  end

  test "turning it on memoizes each type pair once" do
    Comp.memoize_dispatch(true)

    assert Comp.equal?(1, 1)
    one_pair = cached_pairs()
    assert one_pair > 0

    # repeating the same pair caches nothing further
    for _ <- 1..5, do: assert(Comp.equal?(2, 3) == false)
    assert cached_pairs() == one_pair

    # a pair of different types caches more. A comparator may delegate to
    # another pair, so this grows by at least one rather than exactly one.
    _ = Comp.equal?("a", :a)
    assert cached_pairs() > one_pair
  end

  test "reset_dispatch/0 discards what was memoized" do
    Comp.memoize_dispatch(true)
    assert Comp.equal?(1, 1)
    assert cached_pairs() > 0

    assert :ok = Comp.reset_dispatch()
    assert cached_pairs() == 0
  end

  test "toggling clears, so a stale answer cannot survive the switch" do
    Comp.memoize_dispatch(true)
    assert Comp.equal?(1, 1)
    assert cached_pairs() > 0

    Comp.memoize_dispatch(false)
    assert cached_pairs() == 0
  end

  test "answers are the same memoized or not" do
    pairs = [
      {1, 1.0},
      {Decimal.new(1), 1},
      {"a", "a"},
      {Ash.CiString.new("AbC"), Ash.CiString.new("abc")},
      {~U[2020-01-01 00:00:00.000000Z], ~U[2026-01-01 00:00:00.000000Z]},
      {:foo, "foo"}
    ]

    Comp.memoize_dispatch(false)
    without = Enum.map(pairs, fn {l, r} -> Comp.compare(l, r) end)

    Comp.memoize_dispatch(true)
    with_memo = Enum.map(pairs, fn {l, r} -> Comp.compare(l, r) end)
    # and again, now served from the cache
    repeated = Enum.map(pairs, fn {l, r} -> Comp.compare(l, r) end)

    assert without == with_memo
    assert without == repeated
  end
end
