# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Pipeline do
  @moduledoc """
  Represents a reusable pipeline of changes, validations, and preparations
  that can be referenced from multiple actions via `pipe_through`.
  """
  defstruct [
    :name,
    :description,
    changes: [],
    validations: [],
    preparations: [],
    __spark_metadata__: nil
  ]

  @type t :: %__MODULE__{
          name: atom(),
          description: String.t() | nil,
          changes: list(),
          validations: list(),
          preparations: list(),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the pipeline"
    ],
    description: [
      type: :string,
      doc: "An optional description for the pipeline"
    ]
  ]

  def schema, do: @schema
end
