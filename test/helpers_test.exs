defmodule Ash.Test.HelpersTest do
  @moduledoc false
  use ExUnit.Case, async: true

  describe "non_executable_binary_to_term/1" do
    test "it works for simple terms" do
      for data <- [nil, %{}, [], "foo", 1, :blah, 1.1] do
        result =
          data
          |> :erlang.term_to_binary()
          |> Ash.Helpers.non_executable_binary_to_term()

        assert result == data
      end
    end

    test "it fails on functions" do
      func = fn x, y -> x + y end

      binary = :erlang.term_to_binary(func)

      assert_raise ArgumentError, fn ->
        Ash.Helpers.non_executable_binary_to_term(binary)
      end
    end
  end
end
