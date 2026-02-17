# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Where do
  @moduledoc """
  A struct used to store the `where` DSL entity.

  See `d:Reactor.step.where`.
  """

  defstruct __identifier__: nil, description: nil, predicate: nil, __spark_metadata__: nil

  alias Reactor.Guard

  @type t :: %__MODULE__{
          __identifier__: any,
          description: nil | String.t(),
          predicate:
            (Reactor.inputs() -> boolean) | (Reactor.inputs(), Reactor.context() -> boolean),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :where,
      describe: """
      Only execute the surrounding step if the predicate function returns true.

      This is a simple version of `guard` which provides more flexibility at the cost of complexity.
      """,
      examples: [
        """
        step :read_file do
          argument :path, input(:path)
          run &File.read(&1.path)
          where &File.exists?(&1.path)
        end
        """
      ],
      args: [:predicate],
      target: __MODULE__,
      schema: [
        predicate: [
          type: {:or, [{:mfa_or_fun, 1}, {:mfa_or_fun, 2}]},
          required: true,
          doc: """
          Provide a function which takes the step arguments and optionally the context and returns a boolean value.
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

  @doc false
  def eval(arguments, context, where) do
    if eval_predicate(arguments, context, where) do
      :cont
    else
      {:halt, {:ok, nil}}
    end
  end

  defp eval_predicate(arguments, context, where) when is_function(where.predicate, 2),
    do: where.predicate.(arguments, context)

  defp eval_predicate(arguments, _context, where) when is_function(where.predicate, 1),
    do: where.predicate.(arguments)

  defimpl Guard.Build do
    @doc false
    def build(where) do
      {:ok,
       [
         %Guard{fun: {Reactor.Dsl.Where, :eval, [where]}}
       ]}
    end
  end
end
