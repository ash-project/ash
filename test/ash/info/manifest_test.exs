# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.ManifestTest do
  use ExUnit.Case, async: true

  describe "filter/sort capabilities accessors" do
    setup do
      manifest = %Ash.Info.Manifest{
        filter_capabilities: %Ash.Info.Manifest.FilterCapabilities{
          operators: [
            %Ash.Info.Manifest.Operator{name: :==, module: Ash.Query.Operator.Eq},
            %Ash.Info.Manifest.Operator{name: :!=, module: Ash.Query.Operator.NotEq}
          ],
          functions: [
            %Ash.Info.Manifest.Function{name: :contains, module: Ash.Query.Function.Contains}
          ],
          custom_expressions: [
            %Ash.Info.Manifest.CustomExpression{
              name: :jaro,
              module: Ash.Test.Expressions.JaroDistance
            }
          ]
        },
        sort_capabilities: %Ash.Info.Manifest.SortCapabilities{}
      }

      {:ok, manifest: manifest}
    end

    test "operator_lookup/1 returns a name → operator map", %{manifest: manifest} do
      lookup = Ash.Info.Manifest.operator_lookup(manifest)
      assert %Ash.Info.Manifest.Operator{name: :==} = lookup[:==]
      assert %Ash.Info.Manifest.Operator{name: :!=} = lookup[:!=]
    end

    test "function_lookup/1 returns a name → function map", %{manifest: manifest} do
      lookup = Ash.Info.Manifest.function_lookup(manifest)
      assert %Ash.Info.Manifest.Function{name: :contains} = lookup[:contains]
    end

    test "custom_expression_lookup/1 returns a name → custom_expression map", %{
      manifest: manifest
    } do
      lookup = Ash.Info.Manifest.custom_expression_lookup(manifest)
      assert %Ash.Info.Manifest.CustomExpression{name: :jaro} = lookup[:jaro]
    end

    test "lookups handle a nil filter_capabilities" do
      empty = %Ash.Info.Manifest{}
      assert Ash.Info.Manifest.operator_lookup(empty) == %{}
      assert Ash.Info.Manifest.function_lookup(empty) == %{}
      assert Ash.Info.Manifest.custom_expression_lookup(empty) == %{}
    end
  end

  describe "applicable_filter_* per-field lookups" do
    setup do
      {:ok, manifest} = Ash.Info.Manifest.generate(otp_app: :ash_manifest_test)
      {:ok, manifest: manifest}
    end

    test "applicable_filter_operators/2 returns the field's resolved %ApplicableOperator{} list",
         %{manifest: manifest} do
      ops =
        Ash.Info.Manifest.applicable_filter_operators(manifest, {Ash.Test.Manifest.Todo, :title})

      assert is_list(ops)
      assert Enum.any?(ops, &(&1.name == :==))
      assert Enum.all?(ops, &match?(%Ash.Info.Manifest.ApplicableOperator{}, &1))
    end

    test "applicable_filter_functions/2 returns the field's resolved %ApplicableFunction{} list",
         %{manifest: manifest} do
      fns =
        Ash.Info.Manifest.applicable_filter_functions(manifest, {Ash.Test.Manifest.Todo, :title})

      assert is_list(fns)
      assert Enum.all?(fns, &match?(%Ash.Info.Manifest.ApplicableFunction{}, &1))
    end

    test "applicable_filter_custom_expressions/2 returns the field's resolved list",
         %{manifest: manifest} do
      ces =
        Ash.Info.Manifest.applicable_filter_custom_expressions(
          manifest,
          {Ash.Test.Manifest.Todo, :title}
        )

      assert is_list(ces)
    end

    test "lookups raise on unknown resource", %{manifest: manifest} do
      assert_raise RuntimeError, ~r/not found in manifest/, fn ->
        Ash.Info.Manifest.applicable_filter_operators(manifest, {NoSuchResource, :title})
      end
    end

    test "lookups raise on unknown field", %{manifest: manifest} do
      assert_raise RuntimeError, ~r/not found on resource/, fn ->
        Ash.Info.Manifest.applicable_filter_operators(
          manifest,
          {Ash.Test.Manifest.Todo, :no_such_field}
        )
      end
    end
  end
end
