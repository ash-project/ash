# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Calculations.CalculationArgumentErrorsTest do
  use ExUnit.Case, async: true
  require Ash.Query
  import Ash.Expr

  defmodule Domain do
    use Ash.Domain

    resources do
      allow_unregistered? true
    end
  end

  defmodule Label do
    use Ash.Type

    def storage_type(_constraints), do: :string
    def cast_input(value, _constraints) when is_binary(value), do: {:ok, value}
    def cast_input(_value, _constraints), do: :error
    def cast_stored(value, _constraints), do: {:ok, value}
    def dump_to_native(value, _constraints), do: {:ok, value}

    def apply_constraints("constraint-invalid", _constraints), do: :error
    def apply_constraints(value, _constraints), do: {:ok, String.trim(value)}

    def cast_input_array(values, _constraints) do
      if "cast-invalid" in values, do: :error, else: {:ok, values}
    end

    def apply_constraints_array(values, _constraints) do
      cond do
        "constraint-invalid" in values -> :error
        "message-invalid" in values -> {:error, "labels were rejected"}
        true -> {:ok, Enum.map(values, &String.trim/1)}
      end
    end
  end

  defmodule Post do
    use Ash.Resource, domain: Domain, data_layer: Ash.DataLayer.Ets

    ets do
      private? true
    end

    actions do
      defaults [:read, create: :*]
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
    end

    calculations do
      calculate :scalar_matches?, :boolean, expr(title == ^arg(:values)) do
        public? true
        argument :values, Label, allow_nil?: false
      end

      calculate :array_matches?, :boolean, expr(title in ^arg(:values)) do
        public? true
        argument :values, {:array, Label}, allow_nil?: false
      end

      calculate :nested_matches?,
                :boolean,
                expr(^arg(:values) == [["match"]] and title == "match") do
        public? true
        argument :values, {:array, {:array, Label}}, allow_nil?: false
      end
    end
  end

  setup do
    matching = Ash.create!(Post, %{title: "match"})
    other = Ash.create!(Post, %{title: "other"})
    %{matching: matching, other: other}
  end

  defp query(:load, calculation, values) do
    Ash.Query.load(Post, [{calculation, %{values: values}}])
  end

  defp query(:filter, calculation, values) do
    Ash.Query.filter_input(Post, %{
      to_string(calculation) => %{"input" => %{"values" => values}, "eq" => true}
    })
  end

  defp query(:sort, calculation, values) do
    Ash.Query.sort_input(Post, [{to_string(calculation), {%{"values" => values}, :desc}}])
  end

  for {description, calculation, values} <- [
        {"scalar constraints", :scalar_matches?, "constraint-invalid"},
        {"collection casting", :array_matches?, ["cast-invalid"]},
        {"collection constraints", :array_matches?, ["constraint-invalid"]},
        {"nested collection casting", :nested_matches?, [["cast-invalid"]]}
      ],
      operation <- [:load, :filter, :sort] do
    test "#{operation} returns input errors for rejected #{description}" do
      assert {:error, %Ash.Error.Invalid{}} =
               unquote(operation)
               |> query(unquote(calculation), unquote(Macro.escape(values)))
               |> Ash.read()
    end
  end

  for {calculation, values} <- [
        {:scalar_matches?, " match "},
        {:array_matches?, [" match "]},
        {:nested_matches?, [[" match "]]}
      ] do
    test "#{calculation} uses normalized arguments for loads, filters and sorts", context do
      calculation = unquote(calculation)
      values = unquote(Macro.escape(values))

      loaded = query(:load, calculation, values) |> Ash.read!() |> Map.new(&{&1.id, &1})
      assert Map.fetch!(loaded[context.matching.id], calculation)
      refute Map.fetch!(loaded[context.other.id], calculation)

      assert [found] = query(:filter, calculation, values) |> Ash.read!()
      assert found.id == context.matching.id

      assert [first, second] = query(:sort, calculation, values) |> Ash.read!()
      assert first.id == context.matching.id
      assert second.id == context.other.id
    end
  end

  test "collection callback messages survive calculation argument validation" do
    query = query(:load, :array_matches?, ["message-invalid"])
    assert {:error, error} = Ash.read(query)
    assert Exception.message(error) =~ "labels were rejected"
  end
end
