defmodule Ash.Resource.Transformers.NoReservedFieldNames do
  @reserved_names [
    :__struct__,
    :__meta__,
    :__metadata__,
    :__order__,
    :calculations,
    :aggregates
  ]

  @moduledoc """
  Confirms that a resource does not use reserved names for field names.

  Reserved field names are: #{inspect(@reserved_names)}.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
    resource = Transformer.get_persisted(dsl_state, :module)

    attributes =
      dsl_state
      |> Transformer.get_entities([:attributes])

    relationships =
      dsl_state
      |> Transformer.get_entities([:relationships])

    calculations =
      dsl_state
      |> Transformer.get_entities([:calculations])

    aggregates =
      dsl_state
      |> Transformer.get_entities([:aggregates])

    attributes
    |> Enum.concat(relationships)
    |> Enum.concat(calculations)
    |> Enum.concat(aggregates)
    |> Enum.each(fn %{name: name} = field ->
      if name in @reserved_names do
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

          Reserved field names are: #{inspect(@reserved_names)}
          """,
          path: path,
          module: resource
      end
    end)

    {:ok, dsl_state}
  end

  def after_compile?, do: true
end
