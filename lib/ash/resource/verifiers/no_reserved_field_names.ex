# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.NoReservedFieldNames do
  @moduledoc """
  Confirms that a resource does not use reserved names for field names.

  Reserved field names are: #{inspect(Ash.Resource.reserved_names())}.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Entity
  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  def verify(dsl_state) do
    resource = Verifier.get_persisted(dsl_state, :module)

    attributes = Ash.Resource.Info.attributes(dsl_state)
    relationships = Ash.Resource.Info.relationships(dsl_state)
    calculations = Ash.Resource.Info.calculations(dsl_state)
    aggregates = Ash.Resource.Info.aggregates(dsl_state)

    attributes
    |> Enum.concat(relationships)
    |> Enum.concat(calculations)
    |> Enum.concat(aggregates)
    |> Enum.each(fn %{name: name} = field ->
      if name in Ash.Resource.reserved_names() do
        path =
          case field do
            %Ash.Resource.Aggregate{kind: kind} -> [:aggregates, kind, name]
            %Ash.Resource.Calculation{} -> [:calculations, name]
            %Ash.Resource.Attribute{} -> [:attributes, name]
            %{type: type} -> [:relationships, type, name]
          end

        # Get location info from the field entity
        location = Entity.anno(field)

        raise DslError,
          message: """
          Field #{name} is using a reserved name.

          Reserved field names are: #{inspect(Ash.Resource.reserved_names())}
          """,
          path: path,
          location: location,
          module: resource
      end
    end)

    :ok
  end
end
