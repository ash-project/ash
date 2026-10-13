# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Field do
  @moduledoc "Represents an `argument` or `result` of a signal."
  defstruct [
    :name,
    :type,
    :description,
    :default,
    constraints: [],
    allow_nil?: true,
    __spark_metadata__: nil
  ]

  @type t :: %__MODULE__{
          name: atom(),
          type: Ash.Type.t(),
          constraints: Keyword.t(),
          allow_nil?: boolean(),
          default: term(),
          description: String.t() | nil,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the field."
    ],
    type: [
      type: Ash.OptionsHelpers.ash_type(),
      required: true,
      doc: "The type of the field."
    ],
    constraints: [
      type: :keyword_list,
      default: [],
      doc: "Constraints for the type."
    ],
    allow_nil?: [
      type: :boolean,
      default: true,
      doc: "Whether the field may be `nil`."
    ],
    default: [
      type: :any,
      doc: "A default value for the field."
    ],
    description: [
      type: :string,
      doc: "A description of the field."
    ]
  ]

  @doc false
  def schema, do: @schema

  @doc false
  def transform(field) do
    {:ok, %{field | type: Ash.Type.get_type(field.type)}}
  end
end
