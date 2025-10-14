# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.CustomExpressionTest do
  use ExUnit.Case, async: false

  import Ash.Expr

  defmodule ExampleResource do
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: false
    end

    actions do
      defaults [:read, create: [:name]]

      read :matches_query_with_calc do
        argument :query, :string, allow_nil?: false
        filter expr(matches_query?(query: ^arg(:query)))
      end

      read :matches_query_with_explicit_filter do
        argument :query, :string, allow_nil?: false
        filter expr(jaro_distance(name, ^arg(:query)) >= 0.6)
      end
    end

    calculations do
      calculate :matches_query?, :boolean, expr(jaro_distance(name, ^arg(:query)) >= 0.6) do
        argument :query, :string, allow_nil?: false
      end
    end
  end

  test "custom expressions are callable and evaluate" do
    assert eval!(expr(jaro_distance("foo", "bar"))) == String.jaro_distance("foo", "bar")
  end

  test "custom expressions work the same in calculations and without" do
    Ash.create!(ExampleResource, %{name: "foo"})

    assert [_] =
             ExampleResource
             |> Ash.Query.for_read(:matches_query_with_calc, %{query: "fop"})
             |> Ash.read!()

    assert [_] =
             ExampleResource
             |> Ash.Query.for_read(:matches_query_with_explicit_filter, %{query: "fop"})
             |> Ash.read!()
  end
end
