# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Guard do
  @moduledoc """
  A struct used to store the `guard` DSL entity.

  See `d:Reactor.step.guard`
  """

  defstruct __identifier__: nil, description: nil, fun: nil, __spark_metadata__: nil

  alias Reactor.Guard

  @type t :: %__MODULE__{
          __identifier__: any,
          description: nil | String.t(),
          fun: (Reactor.inputs(), Reactor.context() -> :cont | {:halt, Reactor.Step.run_result()}),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :guard,
      describe: """
      Provides a flexible method for conditionally executing a step, or replacing it's result.

      Expects a two arity function which takes the step's arguments and context and returns one of the following:

      - `:cont` - the guard has passed.
      - `{:halt, result}` - the guard has failed - instead of executing the step use the provided result.
      """,
      examples: [
        """
        step :read_file_via_cache do
          argument :path, input(:path)
          run &File.read(&1.path)
          guard fn %{path: path}, %{cache: cache} ->
            case Cache.get(cache, path) do
              {:ok, content} -> {:halt, {:ok, content}}
              _ -> :cont
            end
          end
        end
        """
      ],
      args: [:fun],
      target: __MODULE__,
      schema: [
        fun: [
          type: {:mfa_or_fun, 2},
          required: true,
          doc: """
          The guard function.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description of the guard.
          """
        ]
      ]
    }

  defimpl Guard.Build do
    @doc false
    def build(guard) do
      {:ok, [%Guard{description: guard.description, fun: guard.fun}]}
    end
  end
end
