# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.SingleSegmentTest do
  @moduledoc """
  Phase 1 — single-segment `exists/2` over `{:array, EmbeddedResource}` attribute.

  Verifies in-memory (ETS) filtering, the auto-any shorthand rejection, and
  that paths leading nowhere still produce a usable error.

  See `specs/embedded-array-exists.md`.
  """
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.EmbeddedArray.Estimate

  setup do
    [
      cheap:
        create(%{
          title: "cheap",
          options: [%{name: "basic", total_amt: Decimal.new("50")}]
        }),
      expensive:
        create(%{
          title: "expensive",
          options: [%{name: "premium", total_amt: Decimal.new("150")}]
        }),
      mixed:
        create(%{
          title: "mixed",
          options: [
            %{name: "a", total_amt: Decimal.new("10")},
            %{name: "b", total_amt: Decimal.new("200")}
          ]
        }),
      empty: create(%{title: "empty", options: []})
    ]
  end

  describe "exists/2 over embedded array, single segment" do
    test "filters in-memory", %{cheap: cheap, expensive: expensive, mixed: mixed, empty: empty} do
      results =
        Estimate
        |> Ash.Query.filter(exists(options, total_amt > 100))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert expensive.id in results
      assert mixed.id in results
      refute cheap.id in results
      refute empty.id in results
    end

    test "supports equality on string fields", %{cheap: cheap} do
      results =
        Estimate
        |> Ash.Query.filter(exists(options, name == "basic"))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert cheap.id in results
    end

    test "combines with outer predicates", %{expensive: expensive, mixed: mixed} do
      results =
        Estimate
        |> Ash.Query.filter(active == true and exists(options, total_amt > 100))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert expensive.id in results
      assert mixed.id in results
    end

    test "negation: not exists", %{cheap: cheap, empty: empty} do
      results =
        Estimate
        |> Ash.Query.filter(not exists(options, total_amt > 100))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert cheap.id in results
      assert empty.id in results
    end

    test "empty array does not satisfy exists", %{empty: empty} do
      results =
        Estimate
        |> Ash.Query.filter(exists(options, total_amt > 0))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      refute empty.id in results
    end

    test "interpolation via ^ works", %{expensive: expensive, mixed: mixed} do
      threshold = Decimal.new("100")

      results =
        Estimate
        |> Ash.Query.filter(exists(options, total_amt > ^threshold))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert expensive.id in results
      assert mixed.id in results
    end
  end

  describe "auto-any shorthand is rejected" do
    test "options.total_amt > 100 returns a helpful error pointing to exists/2" do
      err =
        assert_raise Ash.Error.Unknown, fn ->
          Estimate
          |> Ash.Query.filter(options.total_amt > 100)
          |> Ash.read!()
        end

      message = Exception.message(err)
      assert message =~ "embedded array"
      assert message =~ "exists("
      assert message =~ "options"
    end
  end

  defp create(attrs) do
    Estimate
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end
end
