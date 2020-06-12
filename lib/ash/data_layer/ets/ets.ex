defmodule Ash.DataLayer.Ets do
  @moduledoc """
  An ETS (Erlang Term Storage) backed Ash Datalayer, for testing.

  This is used for testing. *Do not use this data layer in production*
  """

  alias Ash.Filter.{And, Eq, In, NotEq, NotIn, Or}

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
    Extension.get_opt(resource, [:ets], :private?)
  end

  defmodule Query do
    @moduledoc false
    defstruct [:resource, :filter, :limit, :sort, relationships: %{}, offset: 0]
  end

  @impl true
  def can?(resource, :async_engine) do
    not private?(resource)
  end

  def can?(_, :transact), do: false

  def can?(_, :composite_primary_key), do: true
  def can?(_, :upsert), do: true
  def can?(_, {:filter, :in}), do: true
  def can?(_, {:filter, :not_in}), do: true
  def can?(_, {:filter, :not_eq}), do: true
  def can?(_, {:filter, :eq}), do: true
  def can?(_, {:filter, :and}), do: true
  def can?(_, {:filter, :or}), do: true
  def can?(_, {:filter, :not}), do: true
  def can?(_, {:filter_related, _}), do: true
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
  def can_query_async?(_), do: false

  @impl true
  def filter(query, filter, _resource) do
    {:ok, %{query | filter: filter}}
  end

  @impl true
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  @impl true
  def run_query(%Query{filter: %Ash.Filter{impossible?: true}}, _), do: {:ok, []}

  @impl true
  def run_query(
        %Query{resource: resource, filter: filter, offset: offset, limit: limit, sort: sort},
        _resource
      ) do
    with {:ok, table} <- wrap_or_create_table(resource),
         {:ok, records} <- ETS.Set.to_list(table) do
      filtered_records =
        records
        |> Enum.map(&elem(&1, 1))
        |> filter_matches(filter)

      offset_records =
        filtered_records
        |> do_sort(sort)
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

  defp filter_matches(records, filter) do
    Enum.filter(records, &matches_filter?(&1, filter))
  end

  defp matches_filter?(record, %{ands: [first | rest]} = filter) do
    matches_filter?(record, first) and matches_filter?(record, %{filter | ands: rest})
  end

  defp matches_filter?(record, %{not: not_filter} = filter) when not is_nil(not_filter) do
    not matches_filter?(record, not_filter) and matches_filter?(record, %{filter | not: nil})
  end

  defp matches_filter?(record, %{ors: [first | rest]} = filter) do
    matches_filter?(record, first) or matches_filter?(record, %{filter | ors: rest})
  end

  defp matches_filter?(record, %{resource: resource} = filter) do
    resource
    |> relationships_to_attribute_filters(filter)
    |> Map.get(:attributes)
    |> Enum.all?(fn {key, predicate} ->
      matches_predicate?(Map.get(record, key), predicate)
    end)
  end

  # alias Ash.Filter.{In, NotIn}

  defp matches_predicate?(value, %Eq{value: predicate_value}) do
    value == predicate_value
  end

  defp matches_predicate?(value, %NotEq{value: predicate_value}) do
    value != predicate_value
  end

  defp matches_predicate?(value, %In{values: predicate_value}) do
    value in predicate_value
  end

  defp matches_predicate?(value, %NotIn{values: predicate_value}) do
    value not in predicate_value
  end

  defp matches_predicate?(value, %And{left: left, right: right}) do
    matches_predicate?(value, left) and matches_predicate?(value, right)
  end

  defp matches_predicate?(value, %Or{left: left, right: right}) do
    matches_predicate?(value, left) or matches_predicate?(value, right)
  end

  defp relationships_to_attribute_filters(_, %{relationships: relationships} = filter)
       when relationships in [nil, %{}] do
    filter
  end

  defp relationships_to_attribute_filters(resource, %{relationships: relationships} = filter) do
    Enum.reduce(relationships, filter, fn {rel, related_filter}, filter ->
      relationship = Ash.relationship(resource, rel)

      {field, parsed_related_filter} = related_ids_filter(relationship, related_filter)

      Ash.Filter.add_to_filter(filter, [{field, parsed_related_filter}])
    end)
  end

  defp related_ids_filter(%{type: :many_to_many} = rel, filter) do
    destination_query = %Query{
      resource: rel.destination,
      filter: filter
    }

    with {:ok, results} <- run_query(destination_query, rel.destination),
         destination_values <- Enum.map(results, &Map.get(&1, rel.destination_field)),
         %{errors: []} = through_filter <-
           Ash.Filter.parse(
             rel.through,
             [
               {rel.destination_field_on_join_table, [in: destination_values]}
             ],
             filter.api
           ),
         {:ok, join_results} <-
           run_query(%Query{resource: rel.through, filter: through_filter}, rel.through) do
      {rel.source_field,
       [in: Enum.map(join_results, &Map.get(&1, rel.source_field_on_join_table))]}
    else
      %{errors: errors} -> {:error, errors}
      {:error, error} -> {:error, error}
    end
  end

  defp related_ids_filter(rel, filter) do
    query = %Query{
      resource: rel.destination,
      filter: filter
    }

    case run_query(query, rel.destination) do
      {:ok, results} ->
        {rel.source_field, [in: Enum.map(results, &Map.get(&1, rel.destination_field))]}

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_sort(results, empty) when empty in [nil, []], do: results

  defp do_sort(results, [{field, direction}]) do
    Enum.sort_by(results, &Map.get(&1, field), direction)
  end

  defp do_sort(results, [{field, direction} | rest]) do
    results
    |> Enum.group_by(&Map.get(&1, field))
    |> Enum.sort_by(fn {key, _value} -> key end, direction)
    |> Enum.flat_map(fn {_, records} ->
      do_sort(records, rest)
    end)
  end

  @impl true
  def upsert(resource, changeset) do
    create(resource, changeset)
  end

  @impl true
  def create(resource, changeset) do
    pkey =
      resource
      |> Ash.primary_key()
      |> Enum.into(%{}, fn attr ->
        {attr, Ecto.Changeset.get_field(changeset, attr)}
      end)

    with {:ok, table} <- wrap_or_create_table(resource),
         record <- Ecto.Changeset.apply_changes(changeset),
         {:ok, _} <- ETS.Set.put(table, {pkey, record}) do
      {:ok, record}
    else
      {:error, error} -> {:error, error}
    end
  end

  @impl true
  def destroy(%resource{} = record) do
    pkey = Map.take(record, Ash.primary_key(resource))

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
