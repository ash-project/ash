defmodule Ash.DataLayer.Simple do
  @moduledoc """
  A data layer that simply returns structs

  This is the data layer that is used under the hood
  by embedded resources, and resources without data layers.
  """

  use Ash.Dsl.Extension, transformers: [], sections: []

  def show_in_docs?, do: false

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
    Ash.Query.put_context(query, :data, data)
  end

  def resource_to_query(resource, api) do
    %Query{data: [], resource: resource, api: api}
  end

  def run_query(%{data_set?: false}, resource) do
    {:error,
     Ash.Error.SimpleDataLayer.NoDataProvided.exception(
       message:
         "No data provided to resource #{resource}\nA common cause of this is not having a data layer for the resource.\n\nYou can create a data layer by including the following in your resource:\n`use Ash.Resource, data_layer: Ash.DataLayer.Ets`"
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

  def limit(query, limit, _) do
    {:ok, %{query | limit: limit}}
  end

  def set_tenant(_, query, _), do: {:ok, query}

  def filter(query, filter, _resource) do
    {:ok, %{query | filter: filter}}
  end

  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  def set_context(_resource, query, context) do
    case Map.fetch(context, :data) do
      {:ok, value} ->
        {:ok, %{query | data_set?: true, data: value || []}}

      :error ->
        {:ok, query}
    end
  end

  def create(_resource, changeset) do
    Ash.Changeset.apply_attributes(changeset)
  end

  def update(_resource, changeset) do
    Ash.Changeset.apply_attributes(changeset)
  end

  def destroy(_resource, _changeset) do
    :ok
  end
end
