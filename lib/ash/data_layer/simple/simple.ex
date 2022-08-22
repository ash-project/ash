defmodule Ash.DataLayer.Simple do
  @moduledoc """
  A data layer that simply returns structs

  This is the data layer that is used under the hood
  by embedded resources, and resources without data layers.
  """

  use Spark.Dsl.Extension, transformers: [], sections: []

  @doc false
  def can?(_, :create), do: true
  def can?(_, :update), do: true
  def can?(_, :destroy), do: true
  def can?(_, :sort), do: true
  def can?(_, :limit), do: true
  def can?(_, {:sort, _}), do: true
  def can?(_, :filter), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, :nested_expressions), do: true
  def can?(_, {:filter_expr, _}), do: true
  def can?(_, :multitenancy), do: true
  def can?(_, _), do: false

  defmodule Query do
    @moduledoc false
    defstruct [:data, :resource, :filter, :api, :limit, sort: [], data_set?: false]
  end

  @doc """
  Sets the data for a query against a data-layer-less resource
  """
  def set_data(query, data) do
    query = Ash.Query.to_query(query)
    Ash.Query.set_context(query, %{data_layer: %{data: %{query.resource => data}}})
  end

  @doc false
  def resource_to_query(resource, api) do
    %Query{data: [], resource: resource, api: api}
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

  def run_query(%{data: data, sort: sort, api: api, filter: filter, limit: limit}, _resource) do
    {:ok,
     data
     |> Enum.filter(&Ash.Filter.Runtime.matches?(api, &1, filter))
     |> Ash.Actions.Sort.runtime_sort(sort)
     |> then(fn data ->
       if limit do
         Enum.take(data, limit)
       else
         data
       end
     end)}
  end

  @doc false
  def limit(query, limit, _) do
    {:ok, %{query | limit: limit}}
  end

  @doc false
  def set_tenant(_, query, _), do: {:ok, query}

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
      {:ok, %{query | data_set?: true, data: resource_data || []}}
    else
      _ ->
        {:ok, query}
    end
  end

  @doc false
  def create(_resource, changeset) do
    Ash.Changeset.apply_attributes(changeset)
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
