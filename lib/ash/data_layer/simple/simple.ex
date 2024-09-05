defmodule Ash.DataLayer.Simple do
  @moduledoc """
  A data layer that returns structs.

  This is the data layer that is used under the hood
  by embedded resources, and resources without data layers.
  """

  use Spark.Dsl.Extension, transformers: [], sections: []

  @behaviour Ash.DataLayer

  @doc false
  def can?(_, :create), do: true
  def can?(_, :bulk_create), do: true
  def can?(_, :update), do: true
  def can?(_, :destroy), do: true
  def can?(_, :sort), do: true
  def can?(_, :limit), do: true
  def can?(_, :offset), do: true
  def can?(_, {:sort, _}), do: true
  def can?(_, :filter), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, :composite_primary_key), do: true
  def can?(_, :nested_expressions), do: true
  def can?(_, {:filter_expr, _}), do: true
  def can?(_, :multitenancy), do: true
  def can?(_, _), do: false

  defmodule Query do
    @moduledoc false
    defstruct [
      :data,
      :resource,
      :filter,
      :domain,
      :limit,
      :offset,
      :tenant,
      sort: [],
      data_set?: false,
      context: %{}
    ]
  end

  @doc """
  Sets the data for a query against a data-layer-less resource
  """
  def set_data(query, data) do
    query = Ash.Query.new(query)
    Ash.Query.set_context(query, %{data_layer: %{data: %{query.resource => data}}})
  end

  @doc false
  def resource_to_query(resource, domain) do
    %Query{data: [], resource: resource, domain: domain}
  end

  @doc false
  def run_query(%{data_set?: false}, resource) do
    {:error,
     Ash.Error.SimpleDataLayer.NoDataProvided.exception(
       message: """
       No data provided to resource #{resource}. Perhaps you are missing a call to `Ash.DataLayer.Simple.set_data/2`?.

       Another common cause of this is failing to add a data layer for a resource. You can add a data layer like so:

       `use Ash.Resource, data_layer: Ash.DataLayer.Ets`
       """
     )}
  end

  def run_query(
        %{
          data: data,
          sort: sort,
          domain: domain,
          filter: filter,
          limit: limit,
          offset: offset,
          tenant: tenant,
          context: context
        },
        _resource
      ) do
    data
    |> do_filter_matches(filter, domain, tenant, context)
    |> case do
      {:ok, results} ->
        {:ok,
         results
         |> Ash.Actions.Sort.runtime_sort(sort, domain: domain)
         |> then(fn data ->
           if offset && offset != 0 do
             Enum.drop(data, offset)
           else
             data
           end
         end)
         |> then(fn data ->
           if limit do
             Enum.take(data, limit)
           else
             data
           end
         end)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_filter_matches(data, filter, domain, tenant, context) do
    Ash.Filter.Runtime.filter_matches(domain, data, filter,
      actor: context[:private][:actor],
      tenant: tenant
    )
  end

  @doc false
  def limit(query, limit, _) do
    {:ok, %{query | limit: limit}}
  end

  @doc false
  def offset(query, offset, _) do
    {:ok, %{query | offset: offset}}
  end

  @doc false
  def set_tenant(_, query, tenant), do: {:ok, %{query | tenant: tenant}}

  @doc false
  def filter(query, filter, _resource) do
    {:ok, %{query | filter: filter}}
  end

  @doc false
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  @doc false
  def set_context(_resource, query, context) do
    with {:ok, data_layer_context} <- Map.fetch(context, :data_layer),
         {:ok, data} <- Map.fetch(data_layer_context, :data),
         {:ok, resource_data} <- Map.fetch(data, query.resource) do
      {:ok, %{query | data_set?: true, data: resource_data || [], context: context}}
    else
      _ ->
        {:ok, query}
    end
  end

  @doc false
  def create(_resource, changeset) do
    Ash.Changeset.apply_attributes(changeset)
  end

  def bulk_create(_resource, stream, options) do
    if options[:return_records?] do
      Enum.reduce_while(stream, {:ok, []}, fn changeset, {:ok, acc} ->
        case Ash.Changeset.apply_attributes(changeset) do
          {:ok, applied} ->
            {:cont,
             {:ok,
              [
                Ash.Resource.put_metadata(
                  applied,
                  :bulk_create_index,
                  changeset.context.bulk_create.index
                )
                | acc
              ]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
      end)
    else
      :ok
    end
  end

  @doc false
  def update(_resource, changeset) do
    Ash.Changeset.apply_attributes(changeset)
  end

  @doc false
  def destroy(_resource, _changeset) do
    :ok
  end
end
