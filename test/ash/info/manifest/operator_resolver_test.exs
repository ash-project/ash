# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Info.Manifest.Generator.OperatorResolverTest do
  use ExUnit.Case, async: false

  alias Ash.Info.Manifest.Generator.{CapabilitiesBuilder, OperatorResolver}

  alias Ash.Info.Manifest.{
    ApplicableFunction,
    ApplicableOperator,
    FilterCapabilities,
    Function,
    Type
  }

  setup do
    original = Application.get_env(:ash, :custom_expressions, [])

    Application.put_env(:ash, :custom_expressions, [
      Ash.Test.Expressions.JaroDistance
    ])

    on_exit(fn -> Application.put_env(:ash, :custom_expressions, original) end)

    {filter_caps, _sort_caps} = CapabilitiesBuilder.build()
    {:ok, caps: filter_caps}
  end

  defp op_names(applicable_ops), do: Enum.map(applicable_ops, & &1.name)
  defp fn_names(applicable_fns), do: Enum.map(applicable_fns, & &1.name)

  describe "resolve/2 returns structured records" do
    test "operators are returned as %ApplicableOperator{name, rhs}", %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {operators, _functions} = OperatorResolver.resolve(string_type, caps)

      assert Enum.all?(operators, &match?(%ApplicableOperator{}, &1))
    end

    test "functions are returned as %ApplicableFunction{name, rhs}", %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {_operators, functions} = OperatorResolver.resolve(string_type, caps)

      assert Enum.all?(functions, &match?(%ApplicableFunction{}, &1))
    end
  end

  describe "resolve/2 — rhs computation" do
    test ":== gets rhs :same on a string field", %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {operators, _} = OperatorResolver.resolve(string_type, caps)
      eq = Enum.find(operators, &(&1.name == :==))

      assert eq.rhs == :same
    end

    test ":in gets rhs {:array, :same}", %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {operators, _} = OperatorResolver.resolve(string_type, caps)
      in_op = Enum.find(operators, &(&1.name == :in))

      assert in_op.rhs == {:array, :same}
    end

    test ":is_nil gets rhs {:concrete, Ash.Type.Boolean}", %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {operators, _} = OperatorResolver.resolve(string_type, caps)
      is_nil = Enum.find(operators, &(&1.name == :is_nil))

      assert is_nil.rhs == {:concrete, Ash.Type.Boolean}
    end

    test ":contains gets rhs concrete-string on a string field", %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {_operators, functions} = OperatorResolver.resolve(string_type, caps)
      contains = Enum.find(functions, &(&1.name == :contains))

      assert contains.rhs == {:concrete, Ash.Type.String}
    end
  end

  describe "resolve/2 — applicability" do
    test "a string field gets equality + membership operators and string predicate functions",
         %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {operators, functions} = OperatorResolver.resolve(string_type, caps)

      assert :== in op_names(operators)
      assert :!= in op_names(operators)
      assert :in in op_names(operators)
      assert :is_nil in op_names(operators)
      assert :contains in fn_names(functions)
      assert :string_starts_with in fn_names(functions)
      assert :string_ends_with in fn_names(functions)
    end

    test "an integer field gets ordered-comparison operators", %{caps: caps} do
      integer_type = %Type{kind: :integer, name: "Integer", module: Ash.Type.Integer}

      {operators, _functions} = OperatorResolver.resolve(integer_type, caps)
      names = op_names(operators)

      assert :== in names
      assert :!= in names
      assert :< in names
      assert :<= in names
      assert :> in names
      assert :>= in names
      assert :in in names
      assert :is_nil in names
    end

    test "every resolved operator is in the catalog's predicate_operators", %{caps: caps} do
      for kind <- [:string, :integer, :boolean, :date, :uuid] do
        type = %Type{kind: kind, name: Atom.to_string(kind)}
        {operators, _functions} = OperatorResolver.resolve(type, caps)

        for %ApplicableOperator{name: op_name} <- operators do
          assert op_name in caps.predicate_operators,
                 "resolved operator #{inspect(op_name)} not in predicate_operators (kind: #{kind})"
        end
      end
    end

    test "every resolved function is in the catalog's predicate_functions", %{caps: caps} do
      for kind <- [:string, :integer, :boolean, :date, :uuid] do
        type = %Type{kind: kind, name: Atom.to_string(kind)}
        {_operators, functions} = OperatorResolver.resolve(type, caps)

        for %ApplicableFunction{name: fn_name} <- functions do
          assert fn_name in caps.predicate_functions,
                 "resolved function #{inspect(fn_name)} not in predicate_functions (kind: #{kind})"
        end
      end
    end

    test "a fully-unknown type returns empty lists rather than nil", %{caps: caps} do
      unknown_type = %Type{kind: :unknown_kind_for_test, name: "Unknown"}

      assert {operators, functions} = OperatorResolver.resolve(unknown_type, caps)
      assert is_list(operators)
      assert is_list(functions)
    end

    test "a NewType field picks up functions whose signatures target the subtype_of base",
         %{caps: caps} do
      newtype = %Type{
        kind: :type_ref,
        name: "EmailString",
        module: Ash.Test.Manifest.Types.EmailString
      }

      {_operators, functions} = OperatorResolver.resolve(newtype, caps)
      names = fn_names(functions)

      assert :contains in names
      assert :string_starts_with in names
      assert :string_ends_with in names
    end

    test "an array-of-string field gets the :has predicate function", %{caps: caps} do
      array_type = %Type{
        kind: :array,
        name: "Array",
        item_type: %Type{kind: :string, name: "String", module: Ash.Type.String}
      }

      {_operators, functions} = OperatorResolver.resolve(array_type, caps)

      assert :has in fn_names(functions)
    end

    test ":has on an array-of-string field gets rhs :same (the array element type)",
         %{caps: caps} do
      array_type = %Type{
        kind: :array,
        name: "Array",
        item_type: %Type{kind: :string, name: "String", module: Ash.Type.String}
      }

      {_operators, functions} = OperatorResolver.resolve(array_type, caps)
      has = Enum.find(functions, &(&1.name == :has))

      assert has.rhs == :same
    end

    test "an array field drops the ordered comparison operators", %{caps: caps} do
      array_type = %Type{
        kind: :array,
        name: "Array",
        item_type: %Type{kind: :string, name: "String", module: Ash.Type.String}
      }

      {operators, _functions} = OperatorResolver.resolve(array_type, caps)
      names = op_names(operators)

      refute :< in names
      refute :> in names
      refute :<= in names
      refute :>= in names

      assert :== in names
      assert :!= in names
      assert :in in names
      assert :is_nil in names
    end

    test "a non-array field still gets the ordered comparison operators", %{caps: caps} do
      integer_type = %Type{kind: :integer, name: "Integer", module: Ash.Type.Integer}

      {operators, _functions} = OperatorResolver.resolve(integer_type, caps)
      names = op_names(operators)

      assert :< in names
      assert :> in names
      assert :<= in names
      assert :>= in names
    end

    test "a boolean field drops the ordered comparison operators", %{caps: caps} do
      boolean_type = %Type{kind: :boolean, name: "Boolean", module: Ash.Type.Boolean}

      {operators, _functions} = OperatorResolver.resolve(boolean_type, caps)
      names = op_names(operators)

      refute :< in names
      refute :> in names
      refute :<= in names
      refute :>= in names

      assert :== in names
      assert :!= in names
      assert :in in names
      assert :is_nil in names
    end
  end

  describe "resolve/3 with data layer scoping" do
    setup do
      {filter_caps, _sort_caps} =
        CapabilitiesBuilder.build(
          resources_by_data_layer: %{
            Ash.Test.Manifest.FakeDataLayer => [Ash.Test.Manifest.Todo]
          }
        )

      {:ok, caps: filter_caps}
    end

    test "a string field whose resource uses the data layer sees that data layer's functions",
         %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {_operators, functions} =
        OperatorResolver.resolve(string_type, caps, Ash.Test.Manifest.FakeDataLayer)

      assert :fake_ilike in fn_names(functions)
    end

    test "a string field whose resource uses a different data layer does NOT see foreign functions",
         %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {_operators, functions} =
        OperatorResolver.resolve(string_type, caps, SomeOtherDataLayer)

      refute :fake_ilike in fn_names(functions)
    end

    test "a string field with no data layer (nil) only sees builtin functions", %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {_operators, functions} = OperatorResolver.resolve(string_type, caps, nil)

      refute :fake_ilike in fn_names(functions)
      assert :contains in fn_names(functions)
    end

    test "a function tagged with a data layer is still returned when its data layer is passed",
         %{caps: caps} do
      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {_, functions} =
        OperatorResolver.resolve(string_type, caps, Ash.Test.Manifest.FakeDataLayer)

      assert :contains in fn_names(functions)
    end

    test "resolve/2 (no data layer) hides data-layer-tagged functions" do
      caps = %FilterCapabilities{
        operators: [],
        functions: [
          %Function{
            name: :fake_ilike,
            module: Ash.Test.Manifest.FakeDataLayerFunction,
            predicate?: true,
            signatures: [],
            returns: :unknown,
            data_layer_module: Ash.Test.Manifest.FakeDataLayer
          }
        ],
        custom_expressions: [],
        boolean_connectives: [:and, :or, :not],
        predicate_operators: [],
        predicate_functions: [:fake_ilike],
        predicate_custom_expressions: []
      }

      string_type = %Type{kind: :string, name: "String", module: Ash.Type.String}

      {_operators, functions} = OperatorResolver.resolve(string_type, caps)

      refute :fake_ilike in fn_names(functions)
    end
  end
end
