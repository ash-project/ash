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
    accept: [],
    arguments: [],
    changes: [],
    validations: [],
    preparations: [],
    __spark_metadata__: nil
  ]

  @type t :: %__MODULE__{
          name: atom(),
          description: String.t() | nil,
          accept: list(atom()),
          arguments: list(),
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
    ],
    accept: [
      type: {:or, [{:wrap_list, :atom}, {:literal, :*}]},
      default: [],
      doc: "A list of attributes to accept. Merged with the action's own accept list."
    ]
  ]

  def schema, do: @schema
end
