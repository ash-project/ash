# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.FunctionTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule Handler do
    @moduledoc false
    use Ash.Resource, data_layer: Ash.DataLayer.Ets, domain: Ash.Test.Domain

    ets do
      private?(true)
    end

    actions do
      default_accept :*
      defaults [:create, :read]
    end

    attributes do
      uuid_primary_key :id
      attribute :callback, :function, constraints: [mfa: true, arity: 2], public?: true
    end
  end

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

  describe "storage_type/1" do
    test "is :binary without the constraint" do
      assert Ash.Type.Function.storage_type([]) == :binary
      assert Ash.Type.Function.storage_type(mfa: false) == :binary
    end

    test "is :string with mfa: true" do
      assert Ash.Type.Function.storage_type(mfa: true) == :string
    end
  end

  describe "persistence with mfa: true" do
    test "dumps an external capture to its capture string" do
      assert {:ok, "&Elixir.Enum.map/2"} =
               Ash.Type.Function.dump_to_native(&Enum.map/2, mfa: true)
    end

    test "dumps an external capture of an Erlang function" do
      assert {:ok, "&erlang.map_size/1"} =
               Ash.Type.Function.dump_to_native(&:erlang.map_size/1, mfa: true)
    end

    test "loads a capture string back into the same function" do
      assert {:ok, "&Elixir.Enum.map/2" = stored} =
               Ash.Type.Function.dump_to_native(&Enum.map/2, mfa: true)

      assert {:ok, fun} = Ash.Type.Function.cast_stored(stored, mfa: true)
      assert fun == (&Enum.map/2)
      assert fun.([1, 2], &(&1 * 2)) == [2, 4]
    end

    test "a capture string naming an unknown module does not load" do
      # `String.to_existing_atom/1` guards against materialising arbitrary atoms
      # from stored strings, so an unknown module or function is refused outright.
      assert :error =
               Ash.Type.Function.cast_stored("&Elixir.DefinitelyNotARealModule.nope/1", mfa: true)
    end

    test "a malformed capture string does not load" do
      assert :error = Ash.Type.Function.cast_stored("&not a capture", mfa: true)
      assert :error = Ash.Type.Function.cast_stored("&Elixir.Enum.map", mfa: true)
    end

    test "captures are late-bound: a known module/function loads, arity resolved at call" do
      # Consistent with storing a reference rather than the implementation, the
      # arity is not checked against exports at load time — the capture binds and
      # would raise only if invoked. Module and function must still be known atoms.
      assert {:ok, fun} = Ash.Type.Function.cast_stored("&Elixir.Enum.map/99", mfa: true)
      assert is_function(fun, 99)
    end
  end

  describe "persistence without the constraint" do
    test "round-trips a function through the binary form" do
      assert {:ok, dumped} = Ash.Type.Function.dump_to_native(&Enum.map/2, [])
      assert is_binary(dumped)
      assert {:ok, fun} = Ash.Type.Function.cast_stored(dumped, [])
      assert fun == (&Enum.map/2)
    end
  end

  describe "reading a legacy binary row after mfa: true is added" do
    test "an old term_to_binary row still loads under the constraint" do
      # A row written before the constraint existed: binary, not a capture string.
      {:ok, legacy} = Ash.Type.Function.dump_to_native(&Enum.map/2, [])

      # cast_stored reads by shape, so it still loads even though the attribute
      # now declares mfa: true.
      assert {:ok, fun} = Ash.Type.Function.cast_stored(legacy, mfa: true)
      assert fun == (&Enum.map/2)
    end
  end

  describe "reconstructing a capture whose module is not loaded" do
    test "reconstructs from the atom alone, but invocation needs the code at call time" do
      defmodule TransientHandler do
        @moduledoc false
        def handle(x), do: x
      end

      {:ok, stored} = Ash.Type.Function.dump_to_native(&TransientHandler.handle/1, mfa: true)

      # Remove the module: a reading VM that does not currently hold this code.
      :code.purge(TransientHandler)
      :code.delete(TransientHandler)
      refute Code.ensure_loaded?(TransientHandler)

      # Reconstruction still succeeds — it needs the module *atom*, not the loaded
      # code. This is what lets a record be read on a node where the function's
      # implementation is absent.
      assert {:ok, fun} = Ash.Type.Function.cast_stored(stored, mfa: true)
      assert is_function(fun, 1)

      # The capture is late-bound, so with the code absent invocation raises
      # rather than silently misbehaving — the caveat the constraint documents.
      assert_raise UndefinedFunctionError, fn -> fun.(1) end
    end
  end

  describe "through a resource with constraints: [mfa: true]" do
    test "stores an external capture and reads it back callable" do
      assert {:ok, created} =
               Handler
               |> Ash.Changeset.for_create(:create, %{callback: &Enum.map/2})
               |> Ash.create()

      assert {:ok, [read]} = Ash.read(Handler)
      assert read.id == created.id
      assert read.callback.([1, 2], &(&1 * 10)) == [10, 20]
    end

    test "refuses a closure at create" do
      assert {:error, %Ash.Error.Invalid{}} =
               Handler
               |> Ash.Changeset.for_create(:create, %{callback: fn a, b -> {a, b} end})
               |> Ash.create()
    end
  end
end
