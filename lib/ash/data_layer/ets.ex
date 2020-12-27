defmodule Ash.DataLayer.Ets do
  @moduledoc """
  An ETS (Erlang Term Storage) backed Ash Datalayer, for testing.

  This is used for testing. *Do not use this data layer in production*
  """

  alias Ash.Actions.Sort

  alias Ash.Query.Operator.{
    Eq,
    GreaterThan,
    GreaterThanOrEqual,
    In,
    IsNil,
    LessThan,
    LessThanOrEqual
  }

  @behaviour Ash.DataLayer

  @ets %Ash.Dsl.Section{
    name: :ets,
    describe: """
    A section for configuring the ets data layer
    """,
    examples: [
      """
      ets do
        # Used in testing
        private? true
      end
      """
    ],
    schema: [
      private?: [
        type: :boolean,
        default: false
      ]
    ]
  }

  use Ash.Dsl.Extension, sections: [@ets]
  alias Ash.Dsl.Extension

  @spec private?(Ash.resource()) :: boolean
  def private?(resource) do
    Extension.get_opt(resource, [:ets], :private?, false, true)
  end

  defmodule Query do
    @moduledoc false
    defstruct [:resource, :filter, :limit, :sort, :tenant, :api, relationships: %{}, offset: 0]
  end

  @impl true
  def can?(resource, :async_engine) do
    not private?(resource)
  end

  def can?(_, :composite_primary_key), do: true
  def can?(_, :multitenancy), do: true
  def can?(_, :upsert), do: true
  def can?(_, :create), do: true
  def can?(_, :read), do: true
  def can?(_, :update), do: true
  def can?(_, :destroy), do: true
  def can?(_, :sort), do: true
  def can?(_, :filter), do: true
  def can?(_, :limit), do: true
  def can?(_, :offset), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, :transact), do: false
  def can?(_, {:filter_operator, %In{}}), do: true
  def can?(_, {:filter_operator, %Eq{}}), do: true
  def can?(_, {:filter_operator, %LessThan{}}), do: true
  def can?(_, {:filter_operator, %GreaterThan{}}), do: true
  def can?(_, {:filter_operator, %LessThanOrEqual{}}), do: true
  def can?(_, {:filter_operator, %GreaterThanOrEqual{}}), do: true
  def can?(_, {:filter_operator, %IsNil{}}), do: true
  def can?(_, {:query_aggregate, :count}), do: true
  def can?(_, {:sort, _}), do: true
  def can?(_, _), do: false

  @impl true
  def resource_to_query(resource, api) do
    %Query{
      resource: resource,
      api: api
    }
  end

  @impl true
  def limit(query, offset, _), do: {:ok, %{query | limit: offset}}

  @impl true
  def offset(query, offset, _), do: {:ok, %{query | offset: offset}}

  @impl true
  def set_tenant(_resource, query, tenant) do
    {:ok, %{query | tenant: tenant}}
  end

  @impl true
  def filter(query, filter, _resource) do
    {:ok, %{query | filter: filter}}
  end

  @impl true
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  @impl true
  def run_aggregate_query(%{api: api} = query, aggregates, resource) do
    case run_query(query, resource) do
      {:ok, results} ->
        Enum.reduce_while(aggregates, {:ok, %{}}, fn
          %{kind: :count, name: name, query: query}, {:ok, acc} ->
            value =
              results
              |> filter_matches(Map.get(query || %{}, :filter), api)
              |> Enum.count()

            {:cont, {:ok, Map.put(acc, name, value)}}

          _, _ ->
            {:halt, {:error, "unsupported aggregate"}}
        end)
    end
  end

  @impl true
  def run_query(
        %Query{
          resource: resource,
          filter: filter,
          offset: offset,
          limit: limit,
          sort: sort,
          tenant: tenant,
          api: api
        },
        _resource
      ) do
    with {:ok, records} <- get_records(resource, tenant),
         filtered_records <- filter_matches(records, filter, api) do
      offset_records =
        filtered_records
        |> Sort.runtime_sort(sort)
        |> Enum.drop(offset || 0)

      limited_records =
        if limit do
          Enum.take(offset_records, limit)
        else
          offset_records
        end

      {:ok, limited_records}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp get_records(resource, tenant) do
    with {:ok, table} <- wrap_or_create_table(resource, tenant),
         {:ok, record_tuples} <- ETS.Set.to_list(table) do
      {:ok, Enum.map(record_tuples, &elem(&1, 1))}
    end
  end

  defp filter_matches(records, nil, _api), do: records

  defp filter_matches(records, filter, api) do
    Enum.filter(records, &Ash.Filter.Runtime.matches?(api, &1, filter))
  end

  @impl true
  def upsert(resource, changeset) do
    update(resource, changeset)
  end

  @impl true
  def create(resource, changeset) do
    pkey =
      resource
      |> Ash.Resource.primary_key()
      |> Enum.into(%{}, fn attr ->
        {attr, Ash.Changeset.get_attribute(changeset, attr)}
      end)

    with {:ok, table} <- wrap_or_create_table(resource, changeset.tenant),
         record <- Ash.Changeset.apply_attributes(changeset),
         {:ok, _} <- ETS.Set.put(table, {pkey, record}) do
      {:ok, record}
    else
      {:error, error} -> {:error, error}
    end
  end

  @impl true
  def destroy(resource, %{data: record} = changeset) do
    pkey = Map.take(record, Ash.Resource.primary_key(resource))

    with {:ok, table} <- wrap_or_create_table(resource, changeset.tenant),
         {:ok, _} <- ETS.Set.delete(table, pkey) do
      :ok
    else
      {:error, error} -> {:error, error}
    end
  end

  @impl true
  def update(resource, changeset) do
    create(resource, changeset)
  end

  # sobelow_skip ["DOS.StringToAtom"]
  defp wrap_or_create_table(resource, tenant) do
    table =
      if tenant do
        String.to_atom(to_string(tenant) <> to_string(resource))
      else
        resource
      end

    case ETS.Set.wrap_existing(table) do
      {:error, :table_not_found} ->
        protection =
          if private?(resource) do
            :private
          else
            :public
          end

        case ETS.Set.new(
               name: table,
               protection: protection,
               ordered: true,
               read_concurrency: true
             ) do
          {:ok, tab} ->
            {:ok, tab}

          {:error, :table_already_exists} ->
            ETS.Set.wrap_existing(table)

          other ->
            other
        end

      {:ok, table} ->
        {:ok, table}

      {:error, other} ->
        {:error, other}
    end
  end
end
