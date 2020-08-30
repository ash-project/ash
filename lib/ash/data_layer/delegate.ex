defmodule Ash.DataLayer.Delegate do
  @moduledoc """
  A data_layer for adding a resource to one API that simply delegates
  to a resource in a different (or the same) API
  """

  @behaviour Ash.DataLayer

  @delegate %Ash.Dsl.Section{
    name: :delegate,
    describe: """
    A section for configuring which resource/api is delegated to
    """,
    schema: [
      to: [
        type: {:custom, __MODULE__, :to_option, []},
        required: true,
        doc: "A tuple of {api, resource} to delegate calls to"
      ],
      base_filter: [
        type: {:custom, __MODULE__, :base_filter_option, []},
        doc: "A filter to apply to queries made against the resource"
      ],
      authorize?: [
        type: :boolean,
        default: false,
        doc:
          "If `true`, calls to the destination api are authorized according to that resource's rules"
      ]
    ]
  }

  def get_delegated(resource) do
    if Ash.Resource.data_layer(resource) == __MODULE__ do
      get_delegated(resource(resource))
    else
      resource
    end
  end

  @doc false
  def to_option({api, resource}) when is_atom(api) and is_atom(resource) do
    {:ok, {api, resource}}
  end

  def to_option(other) do
    {:error, "Expected a tuple of {api, resource}, got: #{inspect(other)}"}
  end

  @doc false
  def base_filter_option(value) do
    if Keyword.keyword?(value) do
      {:ok, value}
    else
      {:error, "Expected a keyword for base_filter, got: #{inspect(value)}"}
    end
  end

  use Ash.Dsl.Extension, sections: [@delegate]
  alias Ash.Dsl.Extension

  def resource(resource) do
    Extension.get_opt(resource, [:delegate], :to, {nil, nil}) |> elem(1)
  end

  def api(resource) do
    Extension.get_opt(resource, [:delegate], :to, {nil, nil}) |> elem(0)
  end

  def authorize?(resource) do
    Extension.get_opt(resource, [:delegate], :authorize?, nil)
  end

  def base_filter(resource) do
    Extension.get_opt(resource, [:delegate], :base_filter, nil)
  end

  defmodule Query do
    @moduledoc false
    defstruct [:resource, :query, :actor, :authorize?]
  end

  @impl true
  def can?(resource, feature) do
    resource
    |> resource()
    |> Ash.Resource.data_layer_can?(feature)
  end

  @impl true
  def resource_to_query(resource) do
    %Query{
      resource: resource,
      query: %Ash.Query{resource: resource(resource)}
    }
  end

  @impl true
  def limit(%{query: query} = source_query, limit, _) do
    {:ok, %{source_query | query: Ash.Query.limit(query, limit)}}
  end

  @impl true
  def offset(%{query: query} = source_query, offset, _) do
    {:ok, %{source_query | query: Ash.Query.offset(query, offset)}}
  end

  @impl true
  def filter(%{query: query} = source_query, filter, resource) do
    case filter do
      %Ash.Filter{} ->
        {:ok,
         %{
           source_query
           | query: Ash.Query.filter(query, %{filter | resource: resource(resource)})
         }}

      filter ->
        {:ok,
         %{
           source_query
           | query: Ash.Query.filter(query, filter)
         }}
    end
  end

  @impl true
  def sort(%{query: query} = source_query, sort, _resource) do
    {:ok, %{source_query | query: Ash.Query.sort(query, sort)}}
  end

  @impl true
  def transaction(resource, fun) do
    Ash.Resource.transaction(resource(resource), fun)
  end

  @impl true
  def rollback(resource, value) do
    Ash.Resource.rollback(resource(resource), value)
  end

  @impl true
  def in_transaction?(resource) do
    Ash.Resource.in_transaction?(resource(resource))
  end

  @impl true
  def add_aggregate(%{query: query} = source_query, aggregate, _) do
    {:ok,
     %{
       source_query
       | query:
           Ash.Query.aggregate(
             query,
             aggregate.name,
             aggregate.kind,
             aggregate.relationship_path,
             aggregate.query
           )
     }}
  end

  @impl true
  def set_context(_resource, query, map) do
    %{query | authorize?: Map.get(map, :authorize?, false), actor: Map.get(map, :author)}
  end

  @impl true
  def run_query(
        %Query{resource: resource, query: query, authorize?: authorize?, actor: actor},
        _resource
      ) do
    api = api(resource)

    query =
      if base_filter(resource) do
        Ash.Query.filter(query, base_filter(resource))
      else
        query
      end

    if authorize?(resource) && authorize? do
      query
      |> Ash.Query.unset([:calculations, :aggregates, :side_load])
      |> api.read(actor: actor, authorize?: true)
    else
      query
      |> Ash.Query.unset([:calculations, :aggregates, :side_load])
      |> api.read()
    end
    |> case do
      {:ok, results} ->
        keys =
          Enum.map(Ash.Resource.attributes(resource), & &1.name) ++
            Enum.map(Ash.Resource.relationships(resource), & &1.name)

        {:ok,
         Enum.map(results, fn result ->
           struct(resource, Map.take(result, keys))
         end)}

      {:error, error} ->
        {:error, error}
    end
  end

  @impl true
  def run_query_with_lateral_join(
        query,
        root_data,
        source_resource,
        destination_resource,
        source,
        destination
      ) do
    Ash.DataLayer.run_query_with_lateral_join(
      query,
      root_data,
      resource(source_resource),
      destination_resource,
      source,
      destination
    )
  end

  @impl true
  def upsert(resource, source_changeset) do
    destination_resource = resource(resource)
    changeset = translate_changeset(destination_resource.__struct__, source_changeset)

    if authorize?(resource) && changeset_authorize?(changeset) do
      api(resource).create(changeset, upsert?: true, actor: actor(changeset), authorize?: true)
    else
      api(resource).create(changeset, upsert?: true)
    end
    |> case do
      {:ok, upserted} ->
        keys =
          Enum.map(Ash.Resource.attributes(resource), & &1.name) ++
            Enum.map(Ash.Resource.relationships(resource), & &1.name)

        {:ok, struct(resource, Map.take(upserted, keys))}

      {:error, error} ->
        {:error, error}
    end
  end

  @impl true
  def create(resource, source_changeset) do
    destination_resource = resource(resource)
    changeset = translate_changeset(destination_resource.__struct__, source_changeset)

    if authorize?(resource) && changeset_authorize?(changeset) do
      api(resource).create(changeset, actor: actor(changeset), authorize?: true)
    else
      api(resource).create(changeset)
    end
    |> case do
      {:ok, created} ->
        keys =
          Enum.map(Ash.Resource.attributes(resource), & &1.name) ++
            Enum.map(Ash.Resource.relationships(resource), & &1.name)

        {:ok, struct(resource, Map.take(created, keys))}

      {:error, error} ->
        {:error, error}
    end
  end

  @impl true
  def destroy(resource, %{data: %resource{} = record} = changeset) do
    destination_api = api(resource)
    pkey = Ash.Resource.primary_key(resource)
    pkey_value = Map.to_list(Map.take(record, pkey))

    case destination_api.get(resource(resource), pkey_value) do
      {:ok, nil} ->
        {:error, "Delegated resource not found"}

      {:error, error} ->
        {:error, error}

      {:ok, to_destroy} ->
        if authorize?(resource) && changeset_authorize?(changeset) do
          api(resource).destroy(Ash.Changeset.new(to_destroy),
            actor: actor(changeset),
            authorize?: true
          )
        else
          api(resource).destroy(Ash.Changeset.new(to_destroy))
        end
    end
  end

  @impl true
  def update(resource, source_changeset) do
    destination_api = api(resource)
    pkey = Ash.Resource.primary_key(resource)
    pkey_value = Map.to_list(Map.take(source_changeset.data, pkey))

    case destination_api.get(resource(resource), pkey_value) do
      {:ok, nil} ->
        {:error, "Delegated resource not found"}

      {:error, error} ->
        {:error, error}

      {:ok, to_update} ->
        changeset = translate_changeset(to_update, source_changeset)

        case api(resource).update(changeset) do
          {:ok, updated} ->
            keys =
              Enum.map(Ash.Resource.attributes(resource), & &1.name) ++
                Enum.map(Ash.Resource.relationships(resource), & &1.name)

            {:ok, struct(resource, Map.take(updated, keys))}

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp changeset_authorize?(%{context: %{authorize?: true}}), do: true
  defp changeset_authorize?(_), do: false

  defp actor(%{context: %{actor: actor}}), do: actor
  defp actor(_), do: nil

  defp translate_changeset(data, source_changeset) do
    changeset = Ash.Changeset.new(data)

    changeset =
      Enum.reduce(source_changeset.attributes, changeset, fn {attr, change}, changeset ->
        Ash.Changeset.change_attribute(changeset, attr, change)
      end)

    Enum.reduce(source_changeset.relationships, changeset, fn {rel, change}, changeset ->
      relationship = Ash.Resource.relationship(changeset.source, rel)

      Enum.reduce(change, changeset, fn
        {:add, to_add}, changeset ->
          Ash.Changeset.append_to_relationship(
            changeset,
            relationship.name,
            List.wrap(to_add)
          )

        {:remove, to_remove}, changeset ->
          Ash.Changeset.remove_from_relationship(changeset, relationship.name, to_remove)

        {:replace, to_replace}, changeset ->
          do_translate_replace(relationship, changeset, to_replace)
      end)
    end)
  end

  defp do_translate_replace(relationship, changeset, to_replace) do
    if relationship.cardinality == :one do
      Ash.Changeset.replace_relationship(
        changeset,
        relationship.name,
        List.wrap(to_replace)
      )
    else
      Ash.Changeset.replace_relationship(changeset, relationship.name, to_replace)
    end
  end
end
