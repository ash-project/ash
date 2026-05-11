# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Type.RecursiveNewTypeCompileTest do
  use ExUnit.Case, async: false

  defmodule RecursiveNode do
    use Ash.Type.NewType,
      subtype_of: :map,
      constraints: [
        fields: [
          id: [type: :uuid, allow_nil?: true],
          name: [type: :string, allow_nil?: false],
          children: [type: {:array, __MODULE__}, allow_nil?: true]
        ]
      ]
  end

  @compile_timeout_ms 15_000

  test "runtime cast of a recursive tree works" do
    assert {:ok, [%{name: "root", children: [%{name: "leaf", children: []}]}]} =
             Ash.Type.cast_input(
               {:array, RecursiveNode},
               [%{name: "root", children: [%{name: "leaf", children: []}]}],
               []
             )
  end

  test "compiling a resource that uses the recursive NewType as an action argument terminates" do
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
                  argument(:nodes, {:array, unquote(RecursiveNode)}, default: [])
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
end
