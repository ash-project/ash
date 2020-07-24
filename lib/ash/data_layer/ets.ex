defmodule Ash.DataLayer.Ets do
  @moduledoc """
  An ETS (Erlang Term Storage) backed Ash Datalayer, for testing.

  This is used for testing. *Do not use this data layer in production*
  """

  alias Ash.Actions.Sort
  alias Ash.Filter.{Expression, Not, Predicate}
  alias Ash.Filter.Predicate.{Eq, GreaterThan, In, LessThan}

  @behaviour Ash.DataLayer

  @ets %Ash.Dsl.Section{
    name: :ets,
    describe: """
    A section for configuring the ets data layer
    """,
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
    defstruct [:resource, :filter, :limit, :sort, relationships: %{}, offset: 0, aggregates: []]
  end

  @impl true
  def can?(resource, :async_engine) do
    not private?(resource)
  end

  def can?(_, :composite_primary_key), do: true
  def can?(_, :upsert), do: true
  def can?(_, :boolean_filter), do: true
  def can?(_, :transact), do: false
  def can?(_, :delete_with_query), do: false
  def can?(_, {:filter_predicate, _, %In{}}), do: true
  def can?(_, {:filter_predicate, _, %Eq{}}), do: true
  def can?(_, {:filter_predicate, _, %LessThan{}}), do: true
  def can?(_, {:filter_predicate, _, %GreaterThan{}}), do: true
  def can?(_, {:sort, _}), do: true
  def can?(_, _), do: false

  @impl true
  def resource_to_query(resource) do
    %Query{
      resource: resource
    }
  end

  @impl true
  def limit(query, offset, _), do: {:ok, %{query | limit: offset}}

  @impl true
  def offset(query, offset, _), do: {:ok, %{query | offset: offset}}

  @impl true
  def filter(query, filter, _resource) do
    {:ok, %{query | filter: filter}}
  end

  @impl true
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  @impl true
  def add_aggregate(query, aggregate, _) do
    {:ok, %{query | aggregates: [aggregate | query.aggregates]}}
  end

  @impl true
  def run_query(
        %Query{resource: resource, filter: filter, offset: offset, limit: limit, sort: sort},
        _resource
      ) do
    with {:ok, records} <- get_records(resource),
         filtered_records <- filter_matches(records, filter) do
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

  defp get_records(resource) do
    with {:ok, table} <- wrap_or_create_table(resource),
         {:ok, record_tuples} <- ETS.Set.to_list(table) do
      {:ok, Enum.map(record_tuples, &elem(&1, 1))}
    end
  end

  def filter_matches(records, nil), do: records

  def filter_matches(records, filter) do
    Enum.filter(records, &matches_filter?(&1, filter.expression))
  end

  defp matches_filter?(_record, nil), do: true
  defp matches_filter?(_record, boolean) when is_boolean(boolean), do: boolean

  defp matches_filter?(
         record,
         %Predicate{
           predicate: predicate,
           attribute: %{name: name},
           relationship_path: []
         }
       ) do
    matches_predicate?(record, name, predicate)
  end

  defp matches_filter?(record, %Expression{op: :and, left: left, right: right}) do
    matches_filter?(record, left) && matches_filter?(record, right)
  end

  defp matches_filter?(record, %Expression{op: :or, left: left, right: right}) do
    matches_filter?(record, left) || matches_filter?(record, right)
  end

  defp matches_filter?(record, %Not{expression: expression}) do
    not matches_filter?(record, expression)
  end

  defp matches_predicate?(record, field, %Eq{value: predicate_value}) do
    Map.fetch(record, field) == {:ok, predicate_value}
  end

  defp matches_predicate?(record, field, %LessThan{value: predicate_value}) do
    case Map.fetch(record, field) do
      {:ok, value} -> value < predicate_value
      :error -> false
    end
  end

  defp matches_predicate?(record, field, %GreaterThan{value: predicate_value}) do
    case Map.fetch(record, field) do
      {:ok, value} -> value > predicate_value
      :error -> false
    end
  end

  defp matches_predicate?(record, field, %In{values: predicate_values}) do
    case Map.fetch(record, field) do
      {:ok, value} -> value in predicate_values
      :error -> false
    end
  end

  @impl true
  def upsert(resource, changeset) do
    create(resource, changeset)
  end

  @impl true
  def create(resource, changeset) do
    pkey =
      resource
      |> Ash.Resource.primary_key()
      |> Enum.into(%{}, fn attr ->
        {attr, Ash.Changeset.get_attribute(changeset, attr)}
      end)

    with {:ok, table} <- wrap_or_create_table(resource),
         record <- Ash.Changeset.apply_attributes(changeset),
         {:ok, _} <- ETS.Set.put(table, {pkey, record}) do
      {:ok, record}
    else
      {:error, error} -> {:error, error}
    end
  end

  @impl true
  def destroy(%resource{} = record) do
    pkey = Map.take(record, Ash.Resource.primary_key(resource))

    with {:ok, table} <- wrap_or_create_table(resource),
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

  defp wrap_or_create_table(resource) do
    case ETS.Set.wrap_existing(resource) do
      {:error, :table_not_found} ->
        protection =
          if private?(resource) do
            :private
          else
            :public
          end

        ETS.Set.new(
          name: resource,
          protection: protection,
          ordered: true,
          read_concurrency: true
        )

      {:ok, table} ->
        {:ok, table}

      {:error, other} ->
        {:error, other}
    end
  end
end
