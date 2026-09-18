# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.ConstraintReferencedTypesTest do
  use ExUnit.Case, async: true

  defmodule Money do
    use Ash.Type

    @impl true
    def storage_type(_constraints), do: :decimal

    @impl true
    def cast_input(value, _constraints), do: {:ok, value}

    @impl true
    def cast_stored(value, _constraints), do: {:ok, value}

    @impl true
    def dump_to_native(value, _constraints), do: {:ok, value}
  end

  defmodule Line do
    defstruct [:amount]
  end

  defp referenced(type, constraints) do
    type
    |> Ash.Type.constraint_referenced_types(constraints)
    |> Enum.map(&elem(&1, 0))
  end

  test "instance_of is not reported" do
    assert [] == referenced(Ash.Type.Struct, instance_of: Line)
  end

  test "nested field types are reported" do
    assert [Money] ==
             referenced(Ash.Type.Struct, instance_of: Line, fields: [amount: [type: Money]])
  end

  test "fields marked init?: false are not reported" do
    assert [] ==
             referenced(Ash.Type.Struct, fields: [amount: [type: Money, init?: false]])
  end

  test "array types nested inside constraints are reported" do
    assert [Money] ==
             referenced(Ash.Type.Struct, fields: [amounts: [type: {:array, Money}]])
  end
end
