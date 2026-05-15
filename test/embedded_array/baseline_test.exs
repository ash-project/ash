# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.BaselineTest do
  @moduledoc """
  Phase 0 baseline test for the embedded-array `exists/2` proposal.

  Records the *current* behavior (before any production-code changes) so that
  later phases can demonstrate the move from "raises" to "filters correctly."

  See `specs/embedded-array-exists.md` for the overall plan.
  """
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.EmbeddedArray.Estimate
  alias Ash.Test.EmbeddedArray.LineItem
  alias Ash.Test.EmbeddedArray.Option

  setup do
    [
      e1: create_estimate(%{title: "small", active: true, options: [
        %{name: "basic", total_amt: Decimal.new("50"),
          line_items: [%{name: "tea", quantity: 1, unit_price: Decimal.new("3")}]}
      ]}),
      e2: create_estimate(%{title: "large", active: true, options: [
        %{name: "premium", total_amt: Decimal.new("150"),
          line_items: [%{name: "espresso machine", quantity: 1, unit_price: Decimal.new("120")}]}
      ]}),
      e3: create_estimate(%{title: "inactive", active: false, options: [
        %{name: "archived", total_amt: Decimal.new("200"), line_items: []}
      ]})
    ]
  end

  describe "fixture sanity" do
    test "Estimate has expected embedded-array attribute types" do
      assert {:array, Option} =
               Ash.Resource.Info.attribute(Estimate, :options).type
               |> normalize_array_type()

      assert {:array, LineItem} =
               Ash.Resource.Info.attribute(Option, :line_items).type
               |> normalize_array_type()
    end

    test "rows are readable without filters", %{e1: e1, e2: e2, e3: e3} do
      ids =
        Estimate
        |> Ash.read!()
        |> Enum.map(& &1.id)
        |> Enum.sort()

      assert ids == Enum.sort([e1.id, e2.id, e3.id])
    end

    test "filtering on a plain attribute works", %{e1: e1, e2: e2} do
      results =
        Estimate
        |> Ash.Query.filter(active == true)
        |> Ash.read!()
        |> Enum.map(& &1.id)
        |> Enum.sort()

      assert results == Enum.sort([e1.id, e2.id])
    end
  end

  describe "current behavior on embedded-array paths (baseline — Phase 1 should change these)" do
    # NOTE: documented here so that Phase 1 can demonstrate the move from "raises" to
    # "filters correctly". The error shape captured is *intentionally precise* — if the
    # error changes shape (different exception class / different choke point) before
    # Phase 1 lands, that itself is a meaningful signal worth investigating.

    test "exists/2 over an embedded-array attribute currently raises in Ash.Filter.add_expression_part" do
      # Choke point today: lib/ash/filter/filter.ex:3045
      # `related/2` cannot resolve a path that begins with an attribute, so
      # `add_expression_part` raises a plain RuntimeError before reaching
      # `expand_through_path/3`.
      err =
        assert_raise RuntimeError, fn ->
          Estimate
          |> Ash.Query.filter(exists(options, total_amt > 100))
          |> Ash.read!()
        end

      assert err.message =~ "Could not determine related resource for `exists/2` expression"
      assert err.message =~ "Path: [:options]"
    end

    test "auto-any shorthand on an embedded-array attribute currently fails with Invalid reference" do
      # Auto-any shorthand never reaches the exists path; it fails earlier in
      # reference resolution.
      err =
        assert_raise Ash.Error.Unknown, fn ->
          Estimate
          |> Ash.Query.filter(options.total_amt > 100)
          |> Ash.read!()
        end

      assert Exception.message(err) =~ "Invalid reference options.total_amt"
    end
  end

  defp create_estimate(attrs) do
    Estimate
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end

  defp normalize_array_type({:array, mod}), do: {:array, mod}
  defp normalize_array_type(other), do: other
end
