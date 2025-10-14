# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Type.LazyInitTest.Example do
  @moduledoc false
  use Ash.Type.NewType,
    subtype_of: :map,
    constraints: [
      fields: [
        condition: [
          type: :string
        ],
        field: [
          type: :string
        ],
        operator: [
          type: :string
        ],
        value: [
          type: :string
        ],
        predicates: [
          type: {:array, Type.LazyInitTest.ExampleNested}
        ]
      ]
    ]

  def graphql_type(_), do: :predicate
  def graphql_input_type(_), do: :predicate_input
end
