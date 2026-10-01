# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.StaticDefaultEscapeTest do
  use ExUnit.Case, async: false

  defmodule CachingType do
    @moduledoc false
    use Ash.Type

    @impl true
    def storage_type(_), do: :map

    @impl true
    def cast_input(nil, _), do: {:ok, nil}

    def cast_input(_value, _constraints), do: {:ok, %{ref: make_ref()}}

    @impl true
    def cast_stored(value, _), do: {:ok, value}

    @impl true
    def dump_to_native(value, _), do: {:ok, value}
  end

  test "compiling a resource whose static default casts to a non-literal value does not crash" do
    # Regression for https://github.com/ash-project/ash/issues/2392: `Ash.Type.set_default/3`
    # replaces a static attribute default with the result of casting it, and persists that
    # cast value into the DSL state. When a type's `cast_input/2` returns something that
    # cannot be represented as a compile-time literal (a reference, pid from another process,
    # local function, etc.), `Spark.Dsl.__before_compile__/1` used to crash trying to
    # `Macro.escape/1` the whole persisted DSL state.
    resource_module = Module.concat(__MODULE__, "Widget#{System.unique_integer([:positive])}")

    {pid, ref} =
      spawn_monitor(fn ->
        Code.eval_quoted(
          quote do
            defmodule unquote(resource_module) do
              use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

              attributes do
                uuid_primary_key(:id)

                attribute :cache_probe, unquote(CachingType) do
                  public?(true)
                  default(:anything)
                end
              end

              actions do
                defaults([:read, :destroy, create: [:cache_probe]])
              end
            end
          end
        )
      end)

    assert_receive {:DOWN, ^ref, :process, ^pid, :normal}, 5_000
  end
end
