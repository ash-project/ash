# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions.PipeThrough do
  @moduledoc "Represents a `pipe_through` declaration on an action."
  defstruct names: [],
            where: [],
            __spark_metadata__: nil

  @typedoc """
  A reference to one or more named pipelines. `:where` contains the
  validations that must pass for the pipeline to apply.
  """
  @type t :: %__MODULE__{
          names: [atom()],
          where: [Ash.Resource.Validation.ref()],
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @schema [
    names: [
      type: {:wrap_list, :atom},
      required: true,
      doc: "The pipeline name(s) to pipe through."
    ],
    where: [
      type:
        {:wrap_list,
         {:spark_function_behaviour, Ash.Resource.Validation, Ash.Resource.Validation.Builtins,
          {Ash.Resource.Validation.Function, 2}}},
      required: false,
      default: [],
      doc:
        "Validations that must pass for this pipeline to apply. If any fail, the pipeline's entities are skipped."
    ]
  ]

  @doc """
  Returns the DSL option schema for `pipe_through` declarations.

  See [Using pipelines in actions](/documentation/topics/actions/actions.md#using-pipelines-in-actions).
  """
  def schema, do: @schema
end
