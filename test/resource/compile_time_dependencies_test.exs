# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Resource.CompileTimeDependenciesTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain, as: Domain
  alias Ash.Test.Support.PolicySimple.Car

  defmodule RunImplementation do
    @moduledoc false
    use Ash.Resource.Actions.Implementation

    def run(_input, _opts, _context), do: {:ok, :ran}
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      authorizers: [Ash.Policy.Authorizer]

    @after_compile __MODULE__

    def __after_compile__(env, _bytecode) do
      {compile, _exports, _runtime, _compile_env} =
        Kernel.LexicalTracker.references(env.lexical_tracker)

      :persistent_term.put({__MODULE__, :compile_references}, compile)
    end

    ets do
      private? true
    end

    attributes do
      uuid_primary_key :id
    end

    actions do
      defaults [:read]

      action :generic, :atom do
        run RunImplementation
      end
    end

    policies do
      policy always() do
        authorize_if accessing_from(Car, :posts)
        authorize_if always()
      end
    end
  end

  defp compile_references do
    :persistent_term.get({Post, :compile_references})
  end

  test "a generic action's `run` module is not a compile-time dependency" do
    refute RunImplementation in compile_references()

    assert {:ok, :ran} =
             Post
             |> Ash.ActionInput.for_action(:generic, %{})
             |> Ash.run_action(authorize?: false)
  end

  test "a module named in a policy check is not a compile-time dependency" do
    refute Car in compile_references()
  end
end
