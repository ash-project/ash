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
end
