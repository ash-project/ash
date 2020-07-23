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

  def requests(query) do
    query.aggregates
    |> Enum.uniq_by(fn {_name, aggregate} ->
      aggregate.relationship_path
    end)
    |> Enum.map(fn {_name, aggregate} ->
      related = Ash.Resource.related(query.resource, aggregate.relationship_path)

      Request.new(
        resource: related,
        api: query.api,
        query:
          Request.resolve(
            [[:data, :query]],
            fn %{
                 data: %{
                   query: data_query
                 }
               } ->
              resource = query.resource

              query = aggregate.query || Ash.Query.new(related)

              relationship =
                Ash.Resource.relationship(resource, List.first(aggregate.relationship_path))

              remaining_path = List.delete_at(aggregate.relationship_path, 0)

              case SideLoad.reverse_relationship_path(relationship, remaining_path) do
                :error ->
                  {:ok, query}

                {:ok, reverse_relationship} ->
                  filter = Ash.Filter.put_at_path(data_query.filter, reverse_relationship)

                  {:ok, Ash.Query.filter(query, filter)}
              end
            end
          ),
        path: [:aggregate, aggregate.relationship_path],
        strict_check_only?: true,
        action: Ash.Resource.primary_action!(query.resource, :read),
        data: []
      )
    end)
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
