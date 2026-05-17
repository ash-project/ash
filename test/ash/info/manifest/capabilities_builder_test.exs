# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.Generator.CapabilitiesBuilderTest do
  use ExUnit.Case, async: false

  alias Ash.Info.Manifest.{
    ArgumentSignature,
    CustomExpression,
    FilterCapabilities,
    Function,
    Operator,
    SortCapabilities
  }

  alias Ash.Info.Manifest.Generator.CapabilitiesBuilder

  setup do
    original = Application.get_env(:ash, :custom_expressions, [])

    Application.put_env(:ash, :custom_expressions, [
      Ash.Test.Expressions.JaroDistance
    ])

    on_exit(fn -> Application.put_env(:ash, :custom_expressions, original) end)

    :ok
  end

  describe "build/0" do
    test "returns a FilterCapabilities and SortCapabilities" do
      {filter_caps, sort_caps} = CapabilitiesBuilder.build()

      assert %FilterCapabilities{} = filter_caps
      assert %SortCapabilities{} = sort_caps
    end

    test "includes every Ash builtin operator" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      catalog_modules = Enum.map(filter_caps.operators, & &1.module)

      for builtin <- Ash.Filter.builtin_operators() do
        assert builtin in catalog_modules,
               "expected operator module #{inspect(builtin)} in catalog"
      end
    end

    test "includes every Ash builtin function" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      catalog_modules = Enum.map(filter_caps.functions, & &1.module)

      for builtin <- Ash.Filter.builtin_functions() do
        assert builtin in catalog_modules,
               "expected function module #{inspect(builtin)} in catalog"
      end
    end

    test "includes registered custom expressions" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      assert Enum.any?(filter_caps.custom_expressions, fn ce ->
               ce.module == Ash.Test.Expressions.JaroDistance and ce.name == :jaro_distance
             end)
    end

    test "operator entries use the user-facing symbol as name with internal name in aliases" do
      {filter_caps, _} = CapabilitiesBuilder.build()
      eq = Enum.find(filter_caps.operators, &(&1.module == Ash.Query.Operator.Eq))

      assert %Operator{} = eq
      # Canonical name is the symbol that appears in `Ash.Query.filter` expressions.
      assert eq.name == :==
      assert :eq in eq.aliases
      assert eq.predicate? == true
      assert is_list(eq.signatures)
      assert Enum.all?(eq.signatures, &match?(%ArgumentSignature{}, &1))
    end

    test "operators without a separate `name` use the operator atom as name and have no aliases" do
      {filter_caps, _} = CapabilitiesBuilder.build()
      in_op = Enum.find(filter_caps.operators, &(&1.module == Ash.Query.Operator.In))

      assert in_op.name == :in
      assert in_op.aliases == []
    end

    test "function entries carry name, module, predicate?, signatures, returns" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      contains =
        Enum.find(filter_caps.functions, &(&1.module == Ash.Query.Function.Contains))

      assert %Function{} = contains
      assert contains.name == :contains
      assert contains.predicate? == true
      assert is_list(contains.signatures)
    end

    test "function with :var_args is preserved" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      fragment =
        Enum.find(filter_caps.functions, &(&1.module == Ash.Query.Function.Fragment))

      if fragment do
        assert fragment.signatures == :var_args
      end
    end

    test "custom expression entries carry name, module, predicate?, signatures" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      jaro =
        Enum.find(filter_caps.custom_expressions, fn ce ->
          ce.module == Ash.Test.Expressions.JaroDistance
        end)

      assert %CustomExpression{} = jaro
      assert jaro.name == :jaro_distance
      assert jaro.predicate? == false
      assert [%ArgumentSignature{args: [a, b]}] = jaro.signatures
      assert a.kind == :concrete
      assert a.builtin == :string
      assert b.kind == :concrete
      assert b.builtin == :string
    end

    test "predicate_operators is the names of predicate? operators" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      expected =
        filter_caps.operators
        |> Enum.filter(& &1.predicate?)
        |> Enum.map(& &1.name)
        |> Enum.sort()

      assert Enum.sort(filter_caps.predicate_operators) == expected
    end

    test "predicate_functions is the names of predicate? functions" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      expected =
        filter_caps.functions
        |> Enum.filter(& &1.predicate?)
        |> Enum.map(& &1.name)
        |> Enum.sort()

      assert Enum.sort(filter_caps.predicate_functions) == expected
    end

    test "predicate_custom_expressions is the names of predicate? custom expressions" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      expected =
        filter_caps.custom_expressions
        |> Enum.filter(& &1.predicate?)
        |> Enum.map(& &1.name)
        |> Enum.sort()

      assert Enum.sort(filter_caps.predicate_custom_expressions) == expected
    end

    test "boolean_connectives is [:and, :or, :not]" do
      {filter_caps, _} = CapabilitiesBuilder.build()
      assert filter_caps.boolean_connectives == [:and, :or, :not]
    end

    test "sort directions are the six standard directions" do
      {_, sort_caps} = CapabilitiesBuilder.build()

      assert sort_caps.directions == [
               :asc,
               :desc,
               :asc_nils_first,
               :asc_nils_last,
               :desc_nils_first,
               :desc_nils_last
             ]
    end
  end

  describe "build/1 with data layer functions" do
    test "collects functions from each data layer and tags them with data_layer_module" do
      {filter_caps, _} =
        CapabilitiesBuilder.build(data_layer_modules: [Ash.Test.Manifest.FakeDataLayer])

      fake =
        Enum.find(
          filter_caps.functions,
          &(&1.module == Ash.Test.Manifest.FakeDataLayerFunction)
        )

      assert %Function{} = fake
      assert fake.name == :fake_ilike
      assert fake.predicate? == true
      assert fake.data_layer_module == Ash.Test.Manifest.FakeDataLayer
    end

    test "predicate_functions includes data-layer-sourced predicates" do
      {filter_caps, _} =
        CapabilitiesBuilder.build(data_layer_modules: [Ash.Test.Manifest.FakeDataLayer])

      assert :fake_ilike in filter_caps.predicate_functions
    end

    test "builtin functions remain untagged (data_layer_module is nil)" do
      {filter_caps, _} = CapabilitiesBuilder.build()

      contains =
        Enum.find(filter_caps.functions, &(&1.module == Ash.Query.Function.Contains))

      assert contains.data_layer_module == nil
    end

    test "deduplicates a function module if multiple data layers list it" do
      {filter_caps, _} =
        CapabilitiesBuilder.build(
          data_layer_modules: [
            Ash.Test.Manifest.FakeDataLayer,
            Ash.Test.Manifest.FakeDataLayer
          ]
        )

      hits =
        Enum.count(filter_caps.functions, &(&1.module == Ash.Test.Manifest.FakeDataLayerFunction))

      assert hits == 1
    end
  end
end
