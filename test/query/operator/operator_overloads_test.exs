# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.OperatorOverloadsTest do
  use ExUnit.Case

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
