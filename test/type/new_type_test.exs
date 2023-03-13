defmodule Ash.Test.Type.NewTypeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule SSN do
    use Ash.Type.NewType,
      subtype_of: :string,
      constraints: [match: ~r/^(?!0{3})(?!6{3})[0-8]\d{2}-(?!0{2})\d{2}-(?!0{4})\d{4}$/]
  end

  test "it handles simple types" do
    assert {:ok, "123-45-6789"} = Ash.Type.cast_input(SSN, "123-45-6789")
  end

  test "it applies the provided constraints" do
    assert {:error,
            message: "must match the pattern %{regex}",
            regex: "~r/^(?!0{3})(?!6{3})[0-8]\\d{2}-(?!0{2})\\d{2}-(?!0{4})\\d{4}$/"} =
             Ash.Type.cast_input(SSN, "hello world")
  end
end
