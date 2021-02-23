defmodule Ash.DataLayer.Simple do
  @moduledoc """
  A data layer that simply returns structs

  This is the data layer that is used under the hood
  by embedded resources
  """

  alias Ash.Query.Operator.{
    Eq,
    GreaterThan,
    GreaterThanOrEqual,
    In,
    IsNil,
    LessThan,
    LessThanOrEqual
  }

  def can?(_, :create), do: true
  def can?(_, :update), do: true
  def can?(_, :destroy), do: true
  def can?(_, :sort), do: true
  def can?(_, {:sort, _}), do: true
  def can?(_, :filter), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, {:filter_operator, %In{}}), do: true
  def can?(_, {:filter_operator, %Eq{}}), do: true
  def can?(_, {:filter_operator, %LessThan{}}), do: true
  def can?(_, {:filter_operator, %GreaterThan{}}), do: true
  def can?(_, {:filter_operator, %LessThanOrEqual{}}), do: true
  def can?(_, {:filter_operator, %GreaterThanOrEqual{}}), do: true
  def can?(_, {:filter_operator, %IsNil{}}), do: true
  def can?(_, _), do: false

  defmodule Query do
    @moduledoc false
    defstruct [:data, :resource, :filter, :api, sort: []]
  end

  def resource_to_query(resource, api) do
    %Query{data: [], resource: resource, api: api}
  end

  def run_query(%{data: data, sort: sort, api: api, filter: filter}, _resource) do
    {:ok,
     data
     |> Enum.filter(&Ash.Filter.Runtime.matches?(api, &1, filter))
     |> Ash.Actions.Sort.runtime_sort(sort)}
  end

  def filter(query, filter, _resource) do
    {:ok, %{query | filter: filter}}
  end

  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  def set_context(_resource, query, context) do
    data = Map.get(context, :data) || []

    {:ok, %{query | data: data}}
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

  @transformers [
    Ash.DataLayer.Simple.Transformers.ValidateDslSections
  ]

  use Ash.Dsl.Extension, transformers: @transformers, sections: []
end
