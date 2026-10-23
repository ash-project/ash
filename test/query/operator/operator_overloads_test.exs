# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.OperatorOverloadsTest do
  use ExUnit.Case

  require Ash.Expr

  alias Ash.Query.Operator

  defmodule TypeWithOverloads do
    use Ash.Type.NewType, subtype_of: :datetime

    @impl true
    def operator_overloads do
      %{
        :> => %{
          [__MODULE__, __MODULE__] => Ash.Type.Boolean,
          [__MODULE__, Ash.Type.UtcDatetime] => {[__MODULE__, __MODULE__], Ash.Type.Boolean}
        }
      }
    end
  end

  defmodule MoneyLike do
    use Ash.Type.NewType, subtype_of: :map

    # Mirrors the comparison overloads AshMoney declares.
    @impl true
    def operator_overloads do
      %{
        :< => %{
          [__MODULE__, :decimal] => Ash.Type.Boolean,
          [:decimal, __MODULE__] => Ash.Type.Boolean,
          [__MODULE__, __MODULE__] => Ash.Type.Boolean
        }
      }
    end
  end

  defmodule Item do
    use Ash.Resource, domain: nil

    attributes do
      uuid_primary_key :id
      attribute :price, MoneyLike, public?: true
      attribute :quantity, :decimal, public?: true
    end
  end

  describe "determine_types/2 with overloads" do
    setup do
      original = Application.get_env(:ash, :known_types, [])
      Application.put_env(:ash, :known_types, [MoneyLike])
      on_exit(fn -> Application.put_env(:ash, :known_types, original) end)
    end

    defp hydrate(expr) do
      {:ok, hydrated} = Ash.Filter.hydrate_refs(expr, %{resource: Item})
      hydrated
    end

    test "an overload does not claim an operand of unknown type" do
      untyped = hydrate(Ash.Expr.expr(fragment("on_hand")))

      for right <- [0, Decimal.new(0)] do
        {types, _returns} = Ash.Expr.determine_types(Operator.LessThan, [untyped, right])

        refute Enum.any?(types, &match?({MoneyLike, _}, &1)),
               "expected no operand typed as MoneyLike for `untyped < #{inspect(right)}`, got: #{inspect(types)}"
      end
    end

    test "an overload does not claim operands only evidenced on its other side" do
      untyped = hydrate(Ash.Expr.expr(fragment("on_hand")))
      quantity = hydrate(Ash.Expr.expr(quantity))

      {types, _returns} = Ash.Expr.determine_types(Operator.LessThan, [untyped, quantity])

      refute Enum.any?(types, &match?({MoneyLike, _}, &1))
    end

    test "an overload applies when an operand is of the type that declares it" do
      price = hydrate(Ash.Expr.expr(price))

      assert {[{MoneyLike, _}, {Ash.Type.Decimal, _}], {Ash.Type.Boolean, _}} =
               Ash.Expr.determine_types(Operator.LessThan, [price, 0])

      untyped = hydrate(Ash.Expr.expr(fragment("amount")))

      assert {[{MoneyLike, _}, {MoneyLike, _}], {Ash.Type.Boolean, _}} =
               Ash.Expr.determine_types(Operator.LessThan, [price, untyped])
    end
  end

  describe "operator_overloads/1" do
    test "does not crash when the same type appears once in known_types" do
      original = Application.get_env(:ash, :known_types, [])

      try do
        Application.put_env(
          :ash,
          :known_types,
          [TypeWithOverloads]
        )

        # This should not raise a BadMapError
        result = Operator.operator_overloads(:>)
        assert is_map(result)
      after
        Application.put_env(:ash, :known_types, original)
      end
    end

    test "does not crash when the same type appears twice in known_types" do
      original = Application.get_env(:ash, :known_types, [])

      try do
        Application.put_env(
          :ash,
          :known_types,
          [TypeWithOverloads, TypeWithOverloads]
        )

        # This should not raise a BadMapError
        result = Operator.operator_overloads(:>)
        assert is_map(result)
      after
        Application.put_env(:ash, :known_types, original)
      end
    end
  end
end
