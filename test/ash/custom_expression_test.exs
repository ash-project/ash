# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.CustomExpressionTest do
  use ExUnit.Case, async: false

  import Ash.Expr
  require Ash.Query

  defmodule CustomInExpression do
    @moduledoc false
    use Ash.CustomExpression,
      name: :custom_in,
      arguments: [
        [:integer, :term]
      ]

    def expression(data_layer, [value, list])
        when data_layer in [
               Ash.DataLayer.Ets,
               Ash.DataLayer.Simple
             ] do
      {:ok, expr(fragment(&__MODULE__.custom_in/2, ^value, ^list))}
    end

    def expression(_data_layer, _args), do: :unknown

    def custom_in(value, %MapSet{} = set), do: MapSet.member?(set, value)
    def custom_in(value, list) when is_list(list), do: value in list
  end

  defmodule IntList do
    @moduledoc false
    use Ash.Type

    @impl true
    def storage_type(_), do: {:array, :integer}

    @impl true
    def cast_input(%MapSet{} = set, constraints), do: cast_input(MapSet.to_list(set), constraints)

    def cast_input(list, _) when is_list(list) do
      if Enum.all?(list, &is_integer/1), do: {:ok, list}, else: :error
    end

    def cast_input(nil, _), do: {:ok, nil}
    def cast_input(_, _), do: :error

    @impl true
    def cast_stored(value, constraints), do: cast_input(value, constraints)

    @impl true
    def dump_to_native(value, _) when is_list(value), do: {:ok, value}
    def dump_to_native(nil, _), do: {:ok, nil}
    def dump_to_native(_, _), do: :error

    @impl true
    def operator_overloads do
      %{
        in: %{
          [:integer, __MODULE__] => __MODULE__
        }
      }
    end

    @impl true
    def evaluate_operator(%Ash.Query.Operator.In{left: left, right: right})
        when is_list(right) do
      {:known, left in right}
    end

    def evaluate_operator(_), do: :unknown

    @impl true
    def operator_expression(%Ash.Query.Operator.In{}) do
      {:ok, CustomInExpression}
    end

    def operator_expression(_), do: :unknown
  end

  defmodule ExampleResource do
    use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, allow_nil?: false
      attribute :score, :integer, allow_nil?: false, default: 0
    end

    actions do
      defaults [:read, create: [:name, :score]]

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

  describe "operator_expression" do
    setup do
      Application.put_env(:ash, :known_types, [IntList])

      Application.put_env(:ash, :custom_expressions, [
        Ash.Test.Expressions.JaroDistance,
        CustomInExpression
      ])

      on_exit(fn ->
        Application.put_env(:ash, :known_types, [])
        Application.put_env(:ash, :custom_expressions, [Ash.Test.Expressions.JaroDistance])
      end)
    end

    test "rewrites operator to custom expression" do
      Ash.create!(ExampleResource, %{name: "a", score: 25})
      Ash.create!(ExampleResource, %{name: "b", score: 5})
      Ash.create!(ExampleResource, %{name: "c", score: 50})

      range = Enum.to_list(20..30)

      results =
        ExampleResource
        |> Ash.Query.filter(expr(score in ^range))
        |> Ash.read!()

      assert length(results) == 1
      assert hd(results).score == 25
    end

    test "falls back to normal operator when no overload matches" do
      Ash.create!(ExampleResource, %{name: "a", score: 1})
      Ash.create!(ExampleResource, %{name: "b", score: 2})

      # Without IntList in known_types, normal `in` behavior
      Application.put_env(:ash, :known_types, [])

      results =
        ExampleResource
        |> Ash.Query.filter(expr(score in [1, 2]))
        |> Ash.read!()

      assert length(results) == 2
    end
  end
end
