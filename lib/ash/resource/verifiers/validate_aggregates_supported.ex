# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateAggregatesSupported do
  @moduledoc """
  Confirms that all aggregates are supported by the data layer
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Entity
  alias Spark.Dsl.Verifier
  alias Spark.Error.DslError

  @impl true
  def verify(dsl_state) do
    resource = Verifier.get_persisted(dsl_state, :module)

    dsl_state
    |> Ash.Resource.Info.aggregates()
    |> Enum.each(fn aggregate ->
      if Map.get(aggregate, :related?, true) do
        check_aggregatable(dsl_state, resource, resource, aggregate, aggregate.relationship_path)
      else
        check_unrelated_aggregate_supported(dsl_state, resource, aggregate)
      end
    end)

    :ok
  end

  defp check_unrelated_aggregate_supported(_dsl_state, resource, aggregate) do
    name = aggregate.name
    kind = aggregate.kind
    location = Entity.anno(aggregate)
    can_do_aggregate_kind? = Ash.DataLayer.data_layer_can?(resource, {:aggregate, kind})
    can_do_unrelated? = Ash.DataLayer.data_layer_can?(resource, {:aggregate, :unrelated})

    cond do
      not can_do_aggregate_kind? ->
        raise DslError,
          module: resource,
          location: location,
          message: "data layer does not support #{kind} aggregates",
          path: [:aggregates, name]

      not can_do_unrelated? ->
        raise DslError,
          module: resource,
          location: location,
          message: "data layer does not support unrelated aggregates",
          path: [:aggregates, name]

      true ->
        :ok
    end
  end

  defp check_aggregatable(_dsl_state, _resource, _root_resource, _aggregate, []), do: :ok

  defp check_aggregatable(dsl_state, resource, root_resource, aggregate, [
         relationship_name | rest
       ]) do
    name = aggregate.name
    location = Entity.anno(aggregate)
    relationship = Ash.Resource.Info.relationship(resource, relationship_name)

    if !relationship do
      raise DslError,
        module: root_resource,
        location: location,
        message:
          "relationship referenced in aggregate `#{inspect(resource)}.#{relationship_name}` does not exist",
        path: [:aggregates, name]
    end

    if Ash.DataLayer.data_layer_can?(
         resource,
         {:aggregate_relationship, relationship}
       ) do
      check_aggregatable(dsl_state, relationship.destination, root_resource, aggregate, rest)
    else
      raise DslError,
        module: root_resource,
        location: location,
        message: "#{inspect(resource)}.#{relationship.name} is not aggregatable",
        path: [:aggregates, name]
    end
  end
end
