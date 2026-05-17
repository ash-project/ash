# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.EdgeCasesTest do
  @moduledoc """
  Phase 5 — edge cases for `exists/2` over embedded array attributes.

  Covers in-memory (ETS) behavior. The AshPostgres equivalents (where
  semantics may differ for nil/empty arrays due to SQL NULL handling)
  live in `ash_postgres/test/embedded_array_exists_test.exs`.

  See `specs/embedded-array-exists.md`.
  """
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.EmbeddedArray.Estimate

  describe "empty / nil arrays" do
    setup do
      [
        empty:
          Estimate
          |> Ash.Changeset.for_create(:create, %{title: "empty", options: []})
          |> Ash.create!(),
        nil_options:
          Estimate
          |> Ash.Changeset.for_create(:create, %{title: "nil_options"})
          |> Ash.create!(),
        populated:
          Estimate
          |> Ash.Changeset.for_create(:create, %{
            title: "populated",
            options: [%{name: "x", total_amt: Decimal.new("10")}]
          })
          |> Ash.create!()
      ]
    end

    test "exists/2 returns false for empty array", %{empty: empty} do
      results =
        Estimate
        |> Ash.Query.filter(exists(options, total_amt > 0))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      refute empty.id in results
    end

    test "exists/2 returns false for nil array", %{nil_options: nil_opts} do
      results =
        Estimate
        |> Ash.Query.filter(exists(options, total_amt > 0))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      refute nil_opts.id in results
    end

    test "not exists/2 returns true for empty and nil arrays", %{
      empty: empty,
      nil_options: nil_opts,
      populated: populated
    } do
      results =
        Estimate
        |> Ash.Query.filter(not exists(options, total_amt > 0))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert empty.id in results
      assert nil_opts.id in results
      refute populated.id in results
    end
  end

  describe "multiple references to the same field in one predicate" do
    setup do
      [
        single_match:
          Estimate
          |> Ash.Changeset.for_create(:create, %{
            title: "single_match",
            options: [%{name: "matchy", total_amt: Decimal.new("50")}]
          })
          |> Ash.create!()
      ]
    end

    test "same attribute referenced twice in the same predicate", %{
      single_match: single_match
    } do
      results =
        Estimate
        |> Ash.Query.filter(exists(options, total_amt > 0 and total_amt < 1000))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert single_match.id in results
    end
  end

end
