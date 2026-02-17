# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.WaitFor do
  @moduledoc """
  The struct used to store `wait_for` DSL entities.

  See `d:Reactor.step.wait_for`.
  """

  defstruct __identifier__: nil, description: nil, names: [], __spark_metadata__: nil

  alias Reactor.{Argument, Dsl}
  import Reactor.Utils

  @type t :: %Dsl.WaitFor{
          names: [atom],
          __identifier__: any,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :wait_for,
      describe: """
      Wait for the named step to complete before allowing this one to start.

      Desugars to `argument :_, result(step_to_wait_for)`
      """,
      examples: ["wait_for :create_user"],
      args: [:names],
      target: Dsl.WaitFor,
      schema: [
        names: [
          type: {:wrap_list, :atom},
          required: true,
          doc: """
          The name of the step to wait for.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description.
          """
        ]
      ]
    }

  defimpl Argument.Build do
    def build(wait_for) do
      wait_for
      |> Map.get(:names, [])
      |> map_while_ok(fn name ->
        {:ok, Argument.from_result(:_, name)}
      end)
    end
  end
end
