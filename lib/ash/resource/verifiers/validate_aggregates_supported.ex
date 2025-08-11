defmodule Ash.Resource.Verifiers.ValidateAggregatesSupported do
  @moduledoc """
  Confirms that all aggregates are supported by the data layer
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    resource = Verifier.get_persisted(dsl_state, :module)

    dsl_state
    |> Verifier.get_entities([:aggregates])
    |> Enum.each(fn aggregate ->
      if Map.get(aggregate, :related?, true) do
        check_aggregatable(resource, resource, aggregate.name, aggregate.relationship_path)
      else
        check_unrelated_aggregate_supported(resource, aggregate.name, aggregate.kind)
      end
    end)

    :ok
  end

  defp check_unrelated_aggregate_supported(resource, name, kind) do
    can_do_aggregate_kind? = Ash.DataLayer.data_layer_can?(resource, {:aggregate, kind})
    can_do_unrelated? = Ash.DataLayer.data_layer_can?(resource, {:aggregate, :unrelated})

    cond do
      not can_do_aggregate_kind? ->
        raise DslError,
          module: resource,
          message: "data layer does not support #{kind} aggregates",
          path: [:aggregates, name]

      not can_do_unrelated? ->
        raise DslError,
          module: resource,
          message: "data layer does not support unrelated aggregates",
          path: [:aggregates, name]

      true ->
        :ok
    end
  end

  defp check_aggregatable(_resource, _root_resource, _name, []), do: :ok

  defp check_aggregatable(resource, root_resource, name, [relationship_name | rest]) do
    relationship = Ash.Resource.Info.relationship(resource, relationship_name)

    if !relationship do
      raise DslError,
        module: root_resource,
        message:
          "relationship referenced in aggregate `#{inspect(resource)}.#{relationship_name}` does not exist",
        path: [:aggregates, name]
    end

    if Ash.DataLayer.data_layer_can?(
         resource,
         {:aggregate_relationship, relationship}
       ) do
      check_aggregatable(relationship.destination, root_resource, name, rest)
    else
      raise DslError,
        module: root_resource,
        message: "#{inspect(resource)}.#{relationship.name} is not aggregatable",
        path: [:aggregates, name]
    end
  end
end
