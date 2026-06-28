# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.EmbeddedArray.NestedTest do
  @moduledoc """
  Phase 3 — nested `exists/2` over `{:array, EmbeddedResource}` attributes and
  `parent/1` scoping.

  Covers in-memory (ETS) behavior. AshPostgres equivalents live in
  `ash_postgres/test/embedded_array_exists_test.exs`.

  See `specs/embedded-array-exists.md`.
  """
  use ExUnit.Case, async: true

  require Ash.Query

  alias Ash.Test.EmbeddedArray.Estimate

  setup do
    [
      with_tea:
        create(%{
          title: "drinks",
          options: [
            %{
              name: "tea-bundle",
              total_amt: Decimal.new("30"),
              line_items: [
                %{name: "tea", quantity: 1, unit_price: Decimal.new("3")},
                %{name: "biscuit", quantity: 2, unit_price: Decimal.new("2")}
              ]
            }
          ]
        }),
      with_coffee:
        create(%{
          title: "drinks",
          options: [
            %{
              name: "coffee-bundle",
              total_amt: Decimal.new("80"),
              line_items: [
                %{name: "coffee", quantity: 1, unit_price: Decimal.new("4")},
                %{name: "muffin", quantity: 1, unit_price: Decimal.new("5")}
              ]
            }
          ]
        }),
      with_matching_names:
        create(%{
          title: "twins",
          options: [
            %{
              name: "matchy",
              total_amt: Decimal.new("10"),
              # innermost name == outer option name
              line_items: [%{name: "matchy", quantity: 1, unit_price: Decimal.new("1")}]
            }
          ]
        })
    ]
  end

  describe "nested exists/2 over embedded arrays" do
    test "exists(options.line_items, name == \"tea\")", %{
      with_tea: with_tea,
      with_coffee: with_coffee
    } do
      results =
        Estimate
        |> Ash.Query.filter(exists(options.line_items, name == "tea"))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert with_tea.id in results
      refute with_coffee.id in results
    end

    test "predicate combines fields of innermost element", %{with_coffee: with_coffee} do
      results =
        Estimate
        |> Ash.Query.filter(exists(options.line_items, name == "muffin" and quantity == 1))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert with_coffee.id in results
    end

    test "explicit nested exists composes: exists(options, exists(line_items, ...))", %{
      with_tea: with_tea,
      with_coffee: with_coffee
    } do
      results =
        Estimate
        |> Ash.Query.filter(exists(options, exists(line_items, name == "biscuit")))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert with_tea.id in results
      refute with_coffee.id in results
    end
  end

  describe "parent/1 refers to the calling scope of the `exists/2` call" do
    # Matches existing Ash convention: `exists/2` pushes the *calling* scope
    # onto the parent stack once, regardless of path length. Note that
    # `exists(a, exists(b, ...))` is auto-flattened by `Ash.Query.Exists.new/3`
    # into `exists(a.b, ...)`, so there is no separate intermediate scope to
    # reach via additional `parent/1` calls — `parent` always lands at the
    # outermost calling resource.

    test "dotted form: parent(...) reaches the calling resource (Estimate)",
         %{with_tea: tea, with_coffee: coffee} do
      results =
        Estimate
        |> Ash.Query.filter(
          exists(options.line_items, name == "tea" and parent(title) == "drinks")
        )
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert tea.id in results
      refute coffee.id in results
    end

    test "explicit nested form auto-flattens; parent still reaches Estimate", %{with_tea: tea} do
      # Behaviorally equivalent to the dotted form thanks to Exists flattening.
      results =
        Estimate
        |> Ash.Query.filter(
          exists(options, exists(line_items, name == "tea" and parent(title) == "drinks"))
        )
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert tea.id in results
    end

    # Mirrors the relationship-exists pattern from
    # `test/filter/parent_test.exs:164` — exercising that `parent/1` can
    # traverse a `belongs_to` relationship from the calling resource and
    # access an attribute on the related row, even when the exists target
    # is an embedded array.
    test "parent can refer to a belongs_to relationship's field" do
      acme =
        Ash.Test.EmbeddedArray.Company
        |> Ash.Changeset.for_create(:create, %{name: "acme"})
        |> Ash.create!()

      other =
        Ash.Test.EmbeddedArray.Company
        |> Ash.Changeset.for_create(:create, %{name: "other"})
        |> Ash.create!()

      matching =
        Estimate
        |> Ash.Changeset.for_create(:create, %{
          title: "matchy",
          company_id: acme.id,
          options: [%{name: "acme", total_amt: Decimal.new("10")}]
        })
        |> Ash.create!()

      _non_matching_co_name =
        Estimate
        |> Ash.Changeset.for_create(:create, %{
          title: "no-match-1",
          company_id: other.id,
          options: [%{name: "acme", total_amt: Decimal.new("10")}]
        })
        |> Ash.create!()

      _non_matching_option_name =
        Estimate
        |> Ash.Changeset.for_create(:create, %{
          title: "no-match-2",
          company_id: acme.id,
          options: [%{name: "something-else", total_amt: Decimal.new("10")}]
        })
        |> Ash.create!()

      results =
        Estimate
        |> Ash.Query.filter(exists(options, name == parent(company.name)))
        |> Ash.read!()
        |> Enum.map(& &1.id)

      assert matching.id in results
      assert length(results) == 1
    end
  end

  defp create(attrs) do
    Estimate
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!()
  end
end
