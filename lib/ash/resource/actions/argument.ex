# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions.Argument do
  @moduledoc "Represents an argument to an action"
  defstruct [
    :allow_nil?,
    :type,
    :name,
    :default,
    :sensitive?,
    :description,
    public?: true,
    constraints: [],
    __spark_metadata__: nil
  ]

  @type t :: %__MODULE__{__spark_metadata__: Spark.Dsl.Entity.spark_meta()}

  def schema do
    [
      name: [
        type: :atom,
        required: true,
        doc: "The name of the argument"
      ],
      type: [
        type: Ash.OptionsHelpers.ash_type(),
        required: true,
        doc: "The type of the argument. See `Ash.Type` for more."
      ],
      description: [
        type: :string,
        doc: "An optional description for the argument."
      ],
      constraints: [
        type: :keyword_list,
        default: [],
        doc: """
        Constraints to provide to the type when casting the value. For more information, see `Ash.Type`.
        """
      ],
      allow_nil?: [
        type: :boolean,
        default: true,
        doc: """
        Whether or not the argument value may be nil (or may be not provided). If nil value is given error is raised.
        """
      ],
      public?: [
        type: :boolean,
        default: true,
        doc: """
        Whether or not the argument should appear in public interfaces
        """
      ],
      sensitive?: [
        type: :boolean,
        default: false,
        doc: """
        Whether or not the argument value contains sensitive information, like PII(Personally Identifiable Information). See the [security guide](/documentation/topics/security/sensitive-data.md) for more.
        """
      ],
      default: [
        type: :any,
        doc:
          "The default value for the argument to take. It can be a zero argument function e.g `&MyMod.my_fun/0` or a value"
      ]
    ]
  end
end
