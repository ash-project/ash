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

  defmodule Inverted do
    use Ash.Type.NewType, subtype_of: :integer

    @impl true
    def operator_overloads, do: %{:> => %{[__MODULE__, __MODULE__] => __MODULE__}}

    @impl true
    def evaluate_operator(%Operator.GreaterThan{left: left, right: right}),
      do: {:known, left < right}
  end

  defmodule ResolvesToBoolean do
    use Ash.Type.NewType, subtype_of: :integer

    @impl true
    def operator_overloads, do: %{:> => %{[__MODULE__, __MODULE__] => Ash.Type.Boolean}}
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

  describe "evaluate/1 with overloads" do
    setup do
      original = Application.get_env(:ash, :known_types, [])
      on_exit(fn -> Application.put_env(:ash, :known_types, original) end)
    end

    test "evaluates through the type an overload resolves to" do
      Application.put_env(:ash, :known_types, [Inverted])

      assert {:known, true} = Operator.evaluate(%Operator.GreaterThan{left: 1, right: 2})
      assert {:known, false} = Operator.evaluate(%Operator.GreaterThan{left: 2, right: 1})
    end

    test "evaluates through the operator when the type an overload resolves to cannot" do
      Application.put_env(:ash, :known_types, [ResolvesToBoolean])
      op = %Operator.GreaterThan{left: 1, right: 2}

      assert {Ash.Type.Boolean, _} = Operator.find_overload(op)
      assert {:known, false} = Operator.evaluate(op)
    end

    test "follows a change to the known types" do
      op = %Operator.GreaterThan{left: 1, right: 2}

      Application.put_env(:ash, :known_types, [Inverted])
      assert {:known, true} = Operator.evaluate(op)

      Application.put_env(:ash, :known_types, [])
      assert {:known, false} = Operator.evaluate(op)

      Application.put_env(:ash, :known_types, [Inverted])
      assert {:known, true} = Operator.evaluate(op)
    end

    test "follows a known type being recompiled" do
      op = %Operator.GreaterThan{left: 1, right: 2}

      defmodule Recompiled do
        use Ash.Type.NewType, subtype_of: :integer

        @impl true
        def operator_overloads, do: %{:> => %{[__MODULE__, __MODULE__] => Ash.Type.Boolean}}
      end

      Application.put_env(:ash, :known_types, [Recompiled])
      assert {:known, false} = Operator.evaluate(op)

      defmodule Recompiled do
        use Ash.Type.NewType, subtype_of: :integer

        @impl true
        def operator_overloads, do: %{:> => %{[__MODULE__, __MODULE__] => __MODULE__}}

        @impl true
        def evaluate_operator(%Operator.GreaterThan{left: left, right: right}),
          do: {:known, left < right}
      end

      assert {:known, true} = Operator.evaluate(op)
    end

    test "follows a module an overload resolves to being defined later" do
      defmodule ResolvesLater do
        use Ash.Type.NewType, subtype_of: :integer

        @impl true
        def operator_overloads,
          do: %{:> => %{[__MODULE__, __MODULE__] => Module.concat(__MODULE__, DefinedLater)}}
      end

      op = %Operator.GreaterThan{left: 1, right: 2}
      Application.put_env(:ash, :known_types, [ResolvesLater])
      assert {:known, false} = Operator.evaluate(op)

      defmodule ResolvesLater.DefinedLater do
        def evaluate_operator(%Operator.GreaterThan{left: left, right: right}),
          do: {:known, left < right}
      end

      assert {:known, true} = Operator.evaluate(op)
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
