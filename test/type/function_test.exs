# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.FunctionTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defp cast_and_apply(value, constraints) do
    with {:ok, value} <- Ash.Type.Function.cast_input(value, constraints) do
      Ash.Type.Function.apply_constraints(value, constraints)
    end
  end

  describe "without the mfa constraint" do
    test "accepts an anonymous function" do
      fun = fn x -> x end
      assert {:ok, ^fun} = cast_and_apply(fun, [])
    end

    test "accepts an external capture" do
      fun = &Enum.map/2
      assert {:ok, ^fun} = cast_and_apply(fun, [])
    end

    test "rejects a non-function" do
      assert :error = cast_and_apply("not a function", [])
    end
  end

  describe "with mfa: true" do
    test "accepts an external capture" do
      fun = &Enum.map/2
      assert {:ok, ^fun} = cast_and_apply(fun, mfa: true)
    end

    test "accepts an external capture of an Erlang function" do
      fun = &:erlang.map_size/1
      assert {:ok, ^fun} = cast_and_apply(fun, mfa: true)
    end

    test "refuses an anonymous function" do
      assert {:error, opts} = cast_and_apply(fn x -> x end, mfa: true)
      assert opts[:message] =~ "capture"
    end

    test "refuses a closure that captures its environment" do
      env = 1
      assert {:error, _} = cast_and_apply(fn x -> x + env end, mfa: true)
    end

    test "refuses a partial application, which is a closure" do
      assert {:error, _} = cast_and_apply(&Enum.map(&1, fn x -> x end), mfa: true)
    end

    test "the cast value stays a directly callable function" do
      assert {:ok, fun} = cast_and_apply(&Enum.map/2, mfa: true)
      assert fun.([1, 2], &(&1 * 2)) == [2, 4]
    end
  end

  describe "mfa combined with arity" do
    test "accepts an external capture of the declared arity" do
      assert {:ok, _} = cast_and_apply(&Enum.map/2, mfa: true, arity: 2)
    end

    test "refuses an external capture of the wrong arity" do
      assert {:error, opts} = cast_and_apply(&Enum.map/2, mfa: true, arity: 1)
      assert opts[:arity] == 1
    end
  end
end
