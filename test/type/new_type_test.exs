# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.NewTypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule SSN do
    use Ash.Type.NewType,
      subtype_of: :string,
      constraints: [match: ~r/^(?!0{3})(?!6{3})[0-8]\d{2}-(?!0{2})\d{2}-(?!0{4})\d{4}$/]
  end

  defmodule CustomMap do
    use Ash.Type.NewType,
      subtype_of: :map,
      constraints: [
        fields: [
          foo: [
            type: :string
          ],
          bar: [
            type: :integer
          ]
        ]
      ]
  end

  defmodule CustomMapWithArray do
    use Ash.Type.NewType,
      subtype_of: :map,
      constraints: [
        fields: [
          custom_map: [
            type: {:array, CustomMap}
          ]
        ]
      ]
  end

  defmodule NativeBird do
    defstruct [:name, :description]

    use Ash.Type.NewType,
      subtype_of: :struct,
      constraints: [
        instance_of: __MODULE__,
        fields: [
          name: [type: :string, allow_nil?: false],
          description: [type: :string, allow_nil?: false]
        ]
      ]
  end

  defmodule LazyNativeBird do
    use Ash.Type.NewType,
      subtype_of: :struct,
      lazy_init?: true,
      constraints: [
        instance_of: NativeBird,
        fields: [
          name: [type: :string, allow_nil?: false],
          description: [type: :string, allow_nil?: false]
        ]
      ]
  end

  test "constraints return the initialized constraints" do
    {:ok, constraints} = Ash.Type.init(NativeBird, [])
    assert constraints == Ash.Type.NewType.constraints(NativeBird, constraints)
  end

  test "constraints return the initialized constraints for lazy initialization" do
    {:ok, constraints} = Ash.Type.init(NativeBird, [])

    assert Enum.sort(constraints) ==
             Enum.sort(Ash.Type.NewType.constraints(LazyNativeBird, constraints))
  end

  test "it handles simple types" do
    assert {:ok, "123-45-6789"} = Ash.Type.cast_input(SSN, "123-45-6789")

    assert {:ok, nil} = Ash.Type.cast_input(SSN, nil)
  end

  test "it applies the provided constraints" do
    assert {:error,
            message: "must match the pattern %{regex}",
            regex: "~r/^(?!0{3})(?!6{3})[0-8]\\d{2}-(?!0{2})\\d{2}-(?!0{4})\\d{4}$/"} =
             Ash.Type.cast_input(SSN, "hello world")
  end

  test "it casts maps correctly" do
    assert {:ok, %{foo: "baz", bar: 42}} ==
             Ash.Type.cast_input(CustomMap, %{"foo" => "baz", "bar" => "42", "other" => "ignored"})
  end

  test "it casts array of maps correctly" do
    assert {:ok, [%{foo: "baz", bar: 42}]} ==
             Ash.Type.cast_input({:array, CustomMap}, [
               %{"foo" => "baz", "bar" => "42", "other" => "ignored"}
             ])
  end

  test "it casts a nested array of maps correctly" do
    assert {:ok, %{custom_map: [%{foo: "baz", bar: 42}]}} ==
             Ash.Type.cast_input(
               CustomMapWithArray,
               %{"custom_map" => [%{"foo" => "baz", "bar" => "42", "other" => "ignored"}]}
             )
  end

  test "returns error for invalid keys" do
    foo = {:foo, [type: :string]}
    bar = {:bar, [type: :integer]}

    assert_raise ArgumentError, ~r/Unknown options given/, fn ->
      defmodule InvalidTypeBySubtype do
        use Ash.Type.NewType,
          subtype_of: :map,
          something: :invalid,
          constraints: [fields: [foo, bar]] ++ [{:something, :invalid}]
      end
    end
  end
end
