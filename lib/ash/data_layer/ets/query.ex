defmodule Ash.DataLayer.Ets.Query do
  @moduledoc """
  Stores a ETS query as it's being built.
  """
  defstruct [
    :resource,
    :filter,
    :limit,
    :sort,
    :tenant,
    :api,
    calculations: [],
    aggregates: [],
    relationships: %{},
    offset: 0
  ]

  @type t :: %__MODULE__{
          resource: Ash.Resource.t(),
          api: Ash.Api.t(),
          filter: nil | Ash.Filter.t(),
          limit: nil | non_neg_integer(),
          sort: nil | Ash.Query.sort(),
          tenant: nil | String.t(),
          calculations: [Ash.Query.Calculation.t()],
          aggregates: [Ash.Query.Aggregate.t()],
          relationships: %{atom => Ash.Resource.Relationships.relationship()},
          offset: non_neg_integer()
        }

  defguardp is_query(query) when is_struct(query, __MODULE__)
  defguardp is_non_neg_integer(number) when is_integer(number) and number >= 0

  @doc "Initialize an empty ETS query."
  @spec init(Ash.Resource.t(), Ash.Api.t()) :: t
  def init(resource, api), do: %__MODULE__{resource: resource, api: api}

  @doc "Add a row limit to the query"
  @spec limit(t, non_neg_integer(), Ash.Resource.t()) :: {:ok, t}
  def limit(query, limit, _) when is_query(query) and is_non_neg_integer(limit),
    do: {:ok, %{query | limit: limit}}

  @doc "Add an offset to the query"
  @spec offset(t, non_neg_integer(), Ash.Resource.t()) :: {:ok, t}
  def offset(query, offset, _) when is_query(query) and is_non_neg_integer(offset),
    do: {:ok, %{query | offset: offset}}

  @doc "Add a filter to the query"
  @spec filter(t, Ash.Filter.t(), Ash.Resource.t()) :: {:ok, t}
  def filter(query, filter, _) when is_query(query) and is_nil(query.filter),
    do: {:ok, %{query | filter: filter}}

  def filter(query, filter, _) when is_query(query),
    do: {:ok, %{query | filter: Ash.Filter.add_to_filter!(query.filter, filter)}}

  @doc "Add a sort to the query"
  @spec sort(t, Ash.Query.sort(), Ash.Resource.t()) :: {:ok, t}
  def sort(query, sort, _) when is_query(query), do: {:ok, %{query | sort: sort}}

  @doc "Add an aggregate to the query"
  @spec add_aggregate(t, Ash.Query.Aggregate.t(), Ash.Resource.t()) :: {:ok, t}
  def add_aggregate(query, aggregate, _)
      when is_query(query) and is_struct(aggregate, Ash.Query.Aggregate),
      do: {:ok, %{query | aggregates: [aggregate | query.aggregates]}}

  @doc "Add a calculation to the query"
  @spec add_calculation(t, Ash.Query.Calculation.t(), any, Ash.Resource.t()) :: {:ok, t}
  def add_calculation(query, calculation, _, _)
      when is_query(query) and is_struct(calculation, Ash.Query.Calculation),
      do: {:ok, %{query | calculations: [calculation | query.calculations]}}

  @doc "Set the tenant for the query"
  @spec set_tenant(Ash.Resource.t(), t, nil | String.t()) :: {:ok, t}
  def set_tenant(_resource, query, tenant)
      when is_query(query) and (is_nil(tenant) or is_binary(tenant)),
      do: {:ok, %{query | tenant: tenant}}
end
