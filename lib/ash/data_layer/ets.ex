defmodule Ash.DataLayer.Ets do
  @moduledoc """
  An ETS (Erlang Term Storage) backed Ash Datalayer, for testing.

  This is used for testing. *Do not use this data layer in production*
  """

  @behaviour Ash.DataLayer

  alias Ash.Filter.{Eq, In, And, Or, NotEq, NotIn}

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @data_layer Ash.DataLayer.Ets

      @ets_private? Keyword.get(opts, :private?, false)

      def ets_private?() do
        @ets_private?
      end
    end
  end

  def private?(resource) do
    resource.ets_private?()
  end

  defmodule Query do
    defstruct [:resource, :filter, :limit, :sort, relationships: %{}, offset: 0]
  end

  @impl true
  def can?(:query_async), do: false
  def can?(:transact), do: false
  def can?(:composite_primary_key), do: true
  def can?({:filter, :in}), do: true
  def can?({:filter, :not_in}), do: true
  def can?({:filter, :not_eq}), do: true
  def can?({:filter, :eq}), do: true
  def can?({:filter, :and}), do: true
  def can?({:filter, :or}), do: true
  def can?({:filter, :not}), do: true
  def can?({:filter_related, _}), do: true
  def can?(_), do: false

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
         {:ok, records} <- ETS.Set.to_list(table),
         records <- Enum.map(records, &elem(&1, 1)),
         {:ok, filtered_records} <-
           filter_matches(records, filter) do
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
    Enum.reduce_while(records, {:ok, []}, fn record, {:ok, acc} ->
      case matches_filter(record, filter) do
        {:ok, true} -> {:cont, {:ok, [record | acc]}}
        {:ok, false} -> {:cont, {:ok, acc}}
        {:error, error} -> {:error, error}
      end
    end)
  end

  defp matches_filter(record, %{ors: empty} = filter) when empty in [nil, []] do
    do_matches_filter(record, filter)
  end

  defp matches_filter(record, %{ors: [first | rest]} = filter) do
    case do_matches_filter(record, first) do
      {:ok, true} -> {:ok, true}
      {:ok, false} -> matches_filter(record, %{filter | ors: rest})
      {:error, error} -> {:error, error}
    end
  end

  defp do_matches_filter(record, filter = %{resource: resource, not: nil}) do
    case relationships_to_attribute_filters(resource, filter) do
      %{errors: [], attributes: attributes} ->
        Enum.reduce_while(attributes, {:ok, true}, fn
          {key, predicate}, {:ok, true} ->
            case matches_predicate?(Map.get(record, key), predicate) do
              {:ok, true} -> {:cont, {:ok, true}}
              {:ok, false} -> {:halt, {:ok, false}}
              {:error, error} -> {:error, error}
            end
        end)

      %{errors: errors} ->
        {:error, errors}
    end
  end

  defp do_matches_filter(record, filter = %{not: not_filter}) do
    case do_matches_filter(record, not_filter) do
      {:ok, true} -> {:ok, false}
      {:ok, false} -> do_matches_filter(record, %{filter | not: nil})
      {:error, error} -> {:error, error}
    end
  end

  # alias Ash.Filter.{In, NotIn}

  defp matches_predicate?(value, %Eq{value: predicate_value}) do
    {:ok, value == predicate_value}
  end

  defp matches_predicate?(value, %NotEq{value: predicate_value}) do
    {:ok, value != predicate_value}
  end

  defp matches_predicate?(value, %In{values: predicate_value}) do
    {:ok, value in predicate_value}
  end

  defp matches_predicate?(value, %NotIn{values: predicate_value}) do
    {:ok, value not in predicate_value}
  end

  defp matches_predicate?(value, %And{left: left, right: right}) do
    case matches_predicate?(value, left) do
      {:ok, true} -> matches_predicate?(value, right)
      {:ok, false} -> {:ok, false}
      {:error, error} -> {:error, error}
    end
  end

  defp matches_predicate?(value, %Or{left: left, right: right}) do
    case matches_predicate?(value, left) do
      {:ok, true} -> {:ok, true}
      {:ok, false} -> matches_predicate?(value, right)
      {:error, error} -> {:error, error}
    end
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
           Ash.Filter.parse(rel.through, [
             {rel.destination_field_on_join_table, [in: destination_values]}
           ]),
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

  defp do_sort(results, [{:asc, field}]) do
    Enum.sort_by(results, &Map.get(&1, field))
  end

  defp do_sort(results, [{:desc, field}]) do
    results |> Enum.sort_by(&Map.get(&1, field)) |> Enum.reverse()
  end

  defp do_sort(results, [{:asc, field} | rest]) do
    results
    |> Enum.group_by(&Map.get(&1, field))
    |> Enum.sort_by(fn {key, _value} -> key end)
    |> Enum.flat_map(fn {_, records} ->
      do_sort(records, rest)
    end)
  end

  defp do_sort(results, [{:desc, field} | rest]) do
    results
    |> Enum.group_by(&Map.get(&1, field))
    |> Enum.sort_by(fn {key, _value} -> key end)
    |> Enum.reverse()
    |> Enum.flat_map(fn {_, records} ->
      do_sort(records, rest)
    end)
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
