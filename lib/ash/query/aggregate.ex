defmodule Ash.Query.Aggregate do
  @moduledoc "Represents an aggregated association value"
  defstruct [
    :name,
    :relationship_path,
    :default_value,
    :query,
    :kind,
    :type,
    :authorization_filter
  ]

  @type t :: %__MODULE__{}

  alias Ash.Actions.SideLoad
  alias Ash.Engine.Request

  def new(_resource, name, kind, relationship, query) do
    with {:ok, type} <- kind_to_type(kind),
         {:ok, query} <- validate_query(query) do
      {:ok,
       %__MODULE__{
         name: name,
         default_value: default_value(kind),
         relationship_path: List.wrap(relationship),
         kind: kind,
         type: type,
         query: query
       }}
    end
  end

  defp default_value(:count), do: 0

  defp validate_query(query) do
    cond do
      query.side_load != [] ->
        {:error, "Cannot side load in an aggregate"}

      query.aggregates != %{} ->
        {:error, "Cannot aggregate in an aggregate"}

      query.sort != [] ->
        {:error, "Cannot sort an aggregate (for now)"}

      not is_nil(query.limit) ->
        {:error, "Cannot limit an aggregate (for now)"}

      not (is_nil(query.offset) || query.offset == 0) ->
        {:error, "Cannot offset an aggregate (for now)"}

      true ->
        {:ok, query}
    end
  end

  defp kind_to_type(:count), do: {:ok, Ash.Type.Integer}
  defp kind_to_type(kind), do: {:error, "Invalid aggregate kind: #{kind}"}

  def requests(initial_query, authorizing?) do
    {auth_requests, value_requests, aggregates_in_query, _} =
      Enum.reduce(initial_query.aggregates, {[], [], [], %{}}, fn {_name, aggregate},
                                                                  {auth_requests, value_requests,
                                                                   aggregates_in_query,
                                                                   added_paths} ->
        related = Ash.Resource.related(initial_query.resource, aggregate.relationship_path)

        query = aggregate.query || Ash.Query.new(related)

        relationship =
          Ash.Resource.relationship(
            initial_query.resource,
            List.first(aggregate.relationship_path)
          )

        remaining_path = List.delete_at(aggregate.relationship_path, 0)

        {in_query?, reverse_relationship} =
          case SideLoad.reverse_relationship_path(relationship, remaining_path) do
            :error ->
              {true, nil}

            {:ok, reverse_relationship} ->
              {any_aggregate_matching_path_used_in_query?(initial_query, aggregate),
               reverse_relationship}
          end

        cond do
          in_query? && !authorizing? ->
            {auth_requests, value_requests, [aggregate | aggregates_in_query], added_paths}

          in_query? && aggregate.relationship_path in added_paths ->
            {auth_requests, value_requests, [aggregate | aggregates_in_query], added_paths}

          in_query? ->
            request =
              Request.new(
                resource: related,
                api: query.api,
                query: aggregate_query(query, reverse_relationship),
                path: [:aggregate, aggregate.name],
                strict_check_only?: true,
                action: Ash.Resource.primary_action!(query.resource, :read),
                data: []
              )

            {[request | auth_requests], value_requests, [aggregate | aggregates_in_query],
             [aggregate.relationship_path | added_paths]}

          true ->
            request = value_request(initial_query, query, reverse_relationship, aggregate)

            {auth_requests, [request | value_requests], aggregates_in_query,
             [aggregate.relationship_path | added_paths]}
        end
      end)

    {auth_requests, value_requests, aggregates_in_query}
  end

  defp value_request(initial_query, query, reverse_relationship, aggregate) do
    pkey = Ash.Resource.primary_key(initial_query.resource)

    Request.new(
      resource: initial_query.resource,
      api: initial_query.api,
      query: aggregate_query(query, reverse_relationship),
      path: [:aggregate_values, aggregate.name],
      action: Ash.Resource.primary_action!(query.resource, :read),
      data:
        Request.resolve(
          [[:data, :data]],
          fn data ->
            if data.data.data == [] do
              {:ok, %{}}
            else
              initial_query = Ash.Query.unset(initial_query, [:filter, :sort, :aggregates])

              query =
                case data.data.data do
                  [record] ->
                    Ash.Query.filter(
                      initial_query,
                      record |> Map.take(pkey) |> Enum.to_list()
                    )

                  records ->
                    Ash.Query.filter(initial_query,
                      or: [Enum.map(records, &Map.take(&1, pkey))]
                    )
                end

              with {:ok, data_layer_query} <-
                     Ash.DataLayer.add_aggregate(
                       query.data_layer_query,
                       aggregate,
                       query.resource
                     ),
                   {:ok, results} <-
                     Ash.DataLayer.run_query(
                       data_layer_query,
                       query.resource
                     ) do
                aggregate_values =
                  Enum.reduce(results, %{}, fn result, acc ->
                    Map.put(
                      acc,
                      Map.take(result, pkey),
                      Map.get(result.aggregates || %{}, aggregate.name)
                    )
                  end)

                {:ok, aggregate_values}
              else
                {:error, error} ->
                  {:error, error}
              end
            end
          end
        )
    )
  end

  defp aggregate_query(query, reverse_relationship) do
    Request.resolve(
      [[:data, :query]],
      fn data ->
        data_query = data.data.query

        filter = Ash.Filter.put_at_path(data_query.filter, reverse_relationship)

        {:ok, Ash.Query.filter(query, filter)}
      end
    )
  end

  defp any_aggregate_matching_path_used_in_query?(query, aggregate) do
    filter_aggregates =
      if query.filter do
        Ash.Filter.used_aggregates(query.filter)
      else
        []
      end

    if Enum.any?(filter_aggregates, &(&1.relationship_path == aggregate.relationship_path)) do
      true
    else
      sort_aggregates =
        Enum.flat_map(query.sort, fn {field, _} ->
          case Map.fetch(query.aggregates, field) do
            :error ->
              []

            {:ok, agg} ->
              [agg]
          end
        end)

      Enum.any?(sort_aggregates, &(&1.relationship_path == aggregate.relationship_path))
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%{query: nil} = aggregate, opts) do
      container_doc(
        "#" <> to_string(aggregate.kind) <> "<",
        [Enum.join(aggregate.relationship_path, ".")],
        ">",
        opts,
        fn str, _ -> str end,
        separator: ""
      )
    end

    def inspect(%{query: query} = aggregate, opts) do
      container_doc(
        "#" <> to_string(aggregate.kind) <> "<",
        [
          concat([
            Enum.join(aggregate.relationship_path, "."),
            concat(" from ", to_doc(query, opts))
          ])
        ],
        ">",
        opts,
        fn str, _ -> str end
      )
    end
  end
end
