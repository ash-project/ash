# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions.PipeThrough do
  @moduledoc "Represents a `pipe_through` declaration on an action."
  defstruct names: [],
            where: [],
            __spark_metadata__: nil

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

  def schema, do: @schema
end
