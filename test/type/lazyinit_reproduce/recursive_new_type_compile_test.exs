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
    resource_module = Module.concat(__MODULE__, "LazyWidget#{System.unique_integer([:positive])}")
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

  test "defining a recursive NewType without `init?: false` warns" do
    import ExUnit.CaptureIO

    module_name = Module.concat(__MODULE__, "Unmarked#{System.unique_integer([:positive])}")

    output =
      capture_io(:stderr, fn ->
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
      end)

    assert output =~ "init?: false"
  end

  test "defining a NewType with a typo'd field type warns" do
    import ExUnit.CaptureIO

    module_name = Module.concat(__MODULE__, "Typo#{System.unique_integer([:positive])}")

    output =
      capture_io(:stderr, fn ->
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
      end)

    assert output =~ "could not be loaded"
  end

  test "the formerly-deadlocking unmarked recursive NewType warns quickly, not by hanging" do
    # Regression for the original deadlock: defining a self-referencing NewType
    # without `init?: false` used to send `Ash.Type.init` into infinite recursion
    # at compile time. It must now produce a warning within a bounded time.
    import ExUnit.CaptureIO

    module_name = Module.concat(__MODULE__, "NoHang#{System.unique_integer([:positive])}")
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        output =
          capture_io(:stderr, fn ->
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
          end)

        send(parent, {:done, output})
      end)

    receive do
      {:done, output} ->
        assert output =~ "init?: false"

      {:DOWN, ^ref, :process, ^pid, reason} ->
        flunk("compile crashed: #{inspect(reason)}")
    after
      @compile_timeout_ms ->
        Process.exit(pid, :kill)

        flunk(
          "unmarked recursive NewType did not finish within #{@compile_timeout_ms}ms — deadlock regression"
        )
    end
  end
end
