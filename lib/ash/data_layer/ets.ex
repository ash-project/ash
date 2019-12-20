defmodule Ash.DataLayer.Ets do
  @moduledoc """
  An ETS (Erlang Term Storage) backed Ash Datalayer.

  This was initially built for testing purposes, since it comes built into OTP.
  This makes it possible to test resources easily, quickly and in isolation from the data layer.
  While this data layer can be used in your application, it should only be used for small/unimportant
  data sets that do not require long term persistence.

  The Ets datalayer *can not perform transactions*. This means that in place updates to many_to_many
  relationships, as well as relationships where the foreign key is stored on the destination table,
  are not possible. Separate requests will have to be made to those resources.
  """

  @behaviour Ash.DataLayer

  alias Ash.Filter.{Eq, In, And, Or}

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
    defstruct [:resource, :filter, :limit, :sort, relationships: %{}, joins: [], offset: 0]
  end

  @impl true
  def can?(:query_async), do: false
  def can?(:transact), do: false
  def can?(:composite_primary_key), do: true
  def can?({:filter, :in}), do: true
  def can?({:filter, :eq}), do: true
  def can?({:filter, :and}), do: true
  def can?({:filter, :or}), do: true
  def can?({:filter_related, _}), do: true
  def can?(_), do: false

  @impl true
  def resource_to_query(resource) do
    %Query{
      resource: resource
    }
  end

  @impl true
  def limit(query, limit, _), do: {:ok, %Query{query | limit: limit}}

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
  def run_query(
        %Query{resource: resource, filter: filter, offset: offset, limit: limit, sort: sort},
        _
      ) do
    query_results =
      filter
      |> all_top_level_queries()
      |> Enum.reduce({:ok, []}, fn query, {:ok, records} ->
        case do_run_query(resource, query, limit + offset) do
          {:ok, results} -> {:ok, records ++ results}
          {:error, error} -> {:error, error}
        end
      end)

    case query_results do
      {:ok, results} ->
        final_results =
          results
          |> Enum.uniq_by(&Map.take(&1, Ash.primary_key(resource)))
          |> do_sort(sort)
          |> Enum.drop(offset)
          |> Enum.take(limit)

        {:ok, final_results}

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_run_query(resource, filter, limit) do
    with %{errors: []} = filter <- relationships_to_attribute_filters(resource, filter),
         {:ok, match_spec} <- filter_to_matchspec(resource, filter),
         {:ok, table} <- wrap_or_create_table(resource),
         {:ok, results} <- match_limit(table, match_spec, limit) do
      {:ok, Enum.map(results, &elem(&1, 1))}
    else
      %{errors: errors} ->
        {:error, errors}

      {:error, error} ->
        {:error, error}
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

  defp related_ids_filter(%{cardinality: :many_to_many} = rel, filter) do
    with {:ok, results} <- do_run_query(rel.destination, filter, nil),
         destination_values <- Enum.map(results, &Map.get(&1, rel.destination_field)),
         %{errors: []} = through_query <-
           Ash.Filter.parse(rel.through, [
             {rel.destination_field_on_join_table, [in: destination_values]}
           ]),
         {:ok, join_results} <- do_run_query(rel.through, through_query, nil) do
      {rel.source_field,
       [in: Enum.map(join_results, &Map.get(&1, rel.source_field_on_join_table))]}
    else
      %{errors: errors} -> {:error, errors}
      {:error, error} -> {:error, error}
    end
  end

  defp related_ids_filter(rel, filter) do
    case do_run_query(rel.destination, filter, nil) do
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

  defp filter_to_matchspec(resource, filter) do
    filter = filter || []

    pkey_match =
      resource
      |> Ash.primary_key()
      |> Enum.reduce(%{}, fn
        _attr, :_ ->
          :_

        attr, pkey_match ->
          case Map.fetch(filter.attributes, attr) do
            {:ok, %Eq{value: value}} ->
              Map.put(pkey_match, attr, value)

            _ ->
              :_
          end
      end)

    starting_matchspec = {{pkey_match, %{__struct__: resource}}, [], [:"$_"]}

    filter
    |> Map.get(:attributes)
    |> Enum.reduce({:ok, {starting_matchspec, %{}}}, fn
      {field, value}, {:ok, {spec, bindings}} ->
        do_filter_to_matchspec(resource, field, value, spec, bindings)

      _, {:error, error} ->
        {:error, error}
    end)
    |> case do
      {:error, error} -> {:error, error}
      {:ok, {spec, _}} -> {:ok, spec}
    end
  end

  defp all_top_level_queries(filter = %{ors: ors}) when is_list(ors) do
    [filter | Enum.flat_map(ors, &all_top_level_queries/1)]
  end

  defp all_top_level_queries(filter), do: [filter]

  # defp attributes_to_expression(%{ors: [first | rest]} = filter, bindings, conditions) do
  #   {left, bindings, conditions} = attributes_to_conditions(first, bindings, conditions)
  #   {right, bindings, conditions} = attributes_to_conditions(rest, bindings, conditions)

  #   {this_expression, bindings, conditions} = attributes_to_conditions(%{filter | ors: []})

  #   {{:orelse, {:orelse, left, right}, this_expression}, bindings, conditions}
  # end

  # defp attributes_to_conditions(%{ors: [first]})

  defp do_filter_to_matchspec(resource, field, value, spec, binding) do
    cond do
      attr = Ash.attribute(resource, field) ->
        do_filter_to_matchspec_attribute(attr.name, value, spec, binding)

      true ->
        {:error, "unsupported filter"}
    end
  end

  defp do_filter_to_matchspec_attribute(
         name,
         value,
         {{id_match, struct_match}, conditions, matcher},
         bindings
       ) do
    case Map.get(bindings, name) do
      nil ->
        binding = bindings |> Map.values() |> Enum.max(fn -> 0 end) |> Kernel.+(1)
        condition = condition(value, binding)

        new_spec =
          {{id_match, Map.put(struct_match, name, :"$#{binding}")}, [condition | conditions],
           matcher}

        {:ok, {new_spec, Map.put(bindings, name, binding)}}

      binding ->
        condition = condition(value, binding)

        new_spec = {{id_match, struct_match}, [condition | conditions], matcher}

        {:ok, new_spec, bindings}
    end
  end

  def condition(%Eq{value: value}, binding) do
    {:==, :"$#{binding}", value}
  end

  def condition(%In{values: []}, _binding) do
    {:==, true, false}
  end

  def condition(%In{values: [value1, value2]}, binding) do
    {:orelse, {:"=:=", :"$#{binding}", value1}, {:"=:=", :"$#{binding}", value2}}
  end

  def condition(%In{values: [value1 | rest]}, binding) do
    {:orelse, condition(%Eq{value: value1}, binding), condition(%In{values: rest}, binding)}
  end

  def condition(%Or{left: left, right: right}, binding) do
    {:orelse, condition(left, binding), condition(right, binding)}
  end

  def condition(%And{left: left, right: right}, binding) do
    {:andalso, condition(left, binding), condition(right, binding)}
  end

  def condition(:in, [], _binding) do
    {:==, false, true}
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
  def update(resource, changeset) do
    create(resource, changeset)
  end

  defp match_limit(table, match_spec, limit) do
    result =
      if limit do
        ETS.Set.select(table, [match_spec], limit)
      else
        ETS.Set.select(table, [match_spec])
      end

    case result do
      {:ok, {matches, _}} -> {:ok, matches}
      {:ok, :"$end_of_table"} -> {:ok, []}
      {:ok, matches} -> {:ok, matches}
      {:error, error} -> {:error, error}
    end
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
