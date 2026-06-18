# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Type.RecursiveNewTypeCompileTest do
  use ExUnit.Case, async: false

  defmodule LazyRecursiveNode do
    use Ash.Type.NewType,
      subtype_of: :map,
      constraints: [
        fields: [
          id: [type: :uuid, allow_nil?: true],
          name: [type: :string, allow_nil?: false],
          children: [type: {:array, __MODULE__}, allow_nil?: true, init?: false]
        ]
      ]
  end

  @compile_timeout_ms 15_000

  test "runtime cast of a recursive tree works" do
    assert {:ok, [%{name: "root", children: [%{name: "leaf", children: []}]}]} =
             Ash.Type.cast_input(
               {:array, LazyRecursiveNode},
               [%{name: "root", children: [%{name: "leaf", children: []}]}],
               []
             )
  end

  test "compiling a resource with `init?: false` on the recursive field terminates" do
    # Compile in a spawned process so a hang fails the test instead of the suite.
    resource_module = Module.concat(__MODULE__, "Widget#{System.unique_integer([:positive])}")
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        Code.eval_quoted(
          quote do
            defmodule unquote(resource_module) do
              use Ash.Resource, domain: Ash.Test.Domain, data_layer: Ash.DataLayer.Ets

              attributes do
                uuid_primary_key(:id)
                attribute(:name, :string, public?: true, allow_nil?: false)
              end

              actions do
                defaults([:read, :destroy, create: [:name]])

                update :save do
                  require_atomic?(false)
                  accept([:name])
                  argument(:nodes, {:array, unquote(LazyRecursiveNode)}, default: [])
                end
              end
            end
          end
        )

        send(parent, :done)
      end)

    receive do
      :done ->
        receive do
          {:DOWN, ^ref, :process, ^pid, _} -> :ok
        after
          1_000 -> :ok
        end

      {:DOWN, ^ref, :process, ^pid, reason} ->
        flunk("resource compilation crashed: #{inspect(reason)}")
    after
      @compile_timeout_ms ->
        Process.exit(pid, :kill)
        flunk("resource compilation did not finish within #{@compile_timeout_ms}ms")
    end
  end

  test "initializing a recursive NewType without `init?: false` raises" do
    module_name = Module.concat(__MODULE__, "Unmarked#{System.unique_integer([:positive])}")

    Code.eval_quoted(
      quote do
        defmodule unquote(module_name) do
          use Ash.Type.NewType,
            subtype_of: :map,
            constraints: [
              fields: [
                name: [type: :string],
                children: [type: {:array, __MODULE__}]
              ]
            ]
        end
      end
    )

    assert_raise ArgumentError, ~r/init\?: false/, fn ->
      Ash.Type.detect_type_cycle!(module_name, [])
    end
  end

  test "initializing a NewType with a typo'd field type raises" do
    module_name = Module.concat(__MODULE__, "Typo#{System.unique_integer([:positive])}")

    Code.eval_quoted(
      quote do
        defmodule unquote(module_name) do
          use Ash.Type.NewType,
            subtype_of: :map,
            constraints: [
              fields: [
                whoops: [type: NoSuchModule]
              ]
            ]
        end
      end
    )

    assert_raise ArgumentError, ~r/could not be loaded/, fn ->
      Ash.Type.detect_type_cycle!(module_name, [])
    end
  end

  test "compile-cycle preflight accepts constraints from module attributes" do
    module_name =
      Module.concat(__MODULE__, "AttrConstraints#{System.unique_integer([:positive])}")

    Code.eval_quoted(
      quote do
        defmodule unquote(module_name) do
          @constraints [
            fields: [
              name: [type: :string],
              children: [type: {:array, __MODULE__}, init?: false]
            ]
          ]

          use Ash.Type.NewType,
            subtype_of: :map,
            constraints: @constraints
        end
      end
    )

    assert {:ok, constraints} = Ash.Type.init(module_name, [])
    assert constraints[:fields][:children][:type] == {:array, module_name}
  end

  test "initializing an unmarked recursive NewType raises quickly, not by hanging" do
    # Regression for the original deadlock: a self-referencing NewType without
    # `init?: false` used to send `Ash.Type.init` into infinite recursion at
    # compile time of any resource using it. It must now raise within a bounded
    # time, with a clear hint to add `init?: false`.
    module_name = Module.concat(__MODULE__, "NoHang#{System.unique_integer([:positive])}")
    parent = self()

    Code.eval_quoted(
      quote do
        defmodule unquote(module_name) do
          use Ash.Type.NewType,
            subtype_of: :map,
            constraints: [
              fields: [
                children: [type: {:array, __MODULE__}]
              ]
            ]
        end
      end
    )

    {pid, ref} =
      spawn_monitor(fn ->
        result =
          try do
            Ash.Type.detect_type_cycle!(module_name, [])
            :ok
          rescue
            e -> {:raised, Exception.message(e)}
          end

        send(parent, {:done, result})
      end)

    receive do
      {:done, {:raised, message}} ->
        assert message =~ "init?: false"

      {:done, :ok} ->
        flunk("expected a raise about init?: false, got successful init")

      {:DOWN, ^ref, :process, ^pid, reason} ->
        flunk("init crashed unexpectedly: #{inspect(reason)}")
    after
      @compile_timeout_ms ->
        Process.exit(pid, :kill)

        flunk(
          "unmarked recursive NewType init did not finish within #{@compile_timeout_ms}ms — deadlock regression"
        )
    end
  end
end
