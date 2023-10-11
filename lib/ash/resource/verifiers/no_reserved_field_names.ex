defmodule Ash.Resource.Verifiers.NoReservedFieldNames do
  @moduledoc """
  Confirms that a resource does not use reserved names for field names.

  Reserved field names are: #{inspect(Ash.Resource.reserved_names())}.
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  def verify(dsl_state) do
    resource = Verifier.get_persisted(dsl_state, :module)

    attributes =
      dsl_state
      |> Verifier.get_entities([:attributes])

    relationships =
      dsl_state
      |> Verifier.get_entities([:relationships])

    calculations =
      dsl_state
      |> Verifier.get_entities([:calculations])

    aggregates =
      dsl_state
      |> Verifier.get_entities([:aggregates])

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

        raise DslError,
          message: """
          Field #{name} is using a reserved name.

          Reserved field names are: #{inspect(Ash.Resource.reserved_names())}
          """,
          path: path,
          module: resource
      end
    end)

    :ok
  end
end
