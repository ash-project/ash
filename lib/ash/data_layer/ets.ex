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
    defstruct [:resource, :filter, :limit, :sort, joins: [], offset: 0]
  end

  @impl true
  def can?(:query_async), do: false
  def can?(:transact), do: false
  def can?(:composite_primary_key), do: true
  def can?({:filter, :in}), do: true
  def can?({:filter, :eq}), do: true
  def can?({:filter, :and}), do: true
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
  def filter(query, filter, resource) do
    # query = %{query | filter: query.filter || []}

    # Enum.reduce(filter, {:ok, query}, fn
    #   _, {:error, error} ->
    #     {:error, error}

    #   {key, value}, {:ok, query} ->
    #     do_filter(query, key, value, resource)
    # end)
    {:ok, query}
  end

  @impl true
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  defp do_filter(query, field, id, _resource) do
    {:ok, %{query | filter: [{field, id} | query.filter]}}
  end

  @impl true
  def run_query(
        %Query{resource: resource, filter: filter, offset: offset, limit: limit, sort: sort},
        _
      ) do
    with {:ok, match_spec} <- filter_to_matchspec(resource, filter),
         {:ok, table} <- wrap_or_create_table(resource),
         {:ok, results} <- match_limit(table, match_spec, limit, offset),
         records <- Enum.map(results, &elem(&1, 1)),
         sorted <- do_sort(records, sort),
         without_offset <- Enum.drop(sorted, offset) do
      {:ok, without_offset}
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

  # Id matching would be great here for performance, but
  # for behaviour is technically unnecessary
  defp filter_to_matchspec(resource, filter) do
    filter = filter || []

    {pkey_match, pkey_names} =
      resource
      |> Ash.primary_key()
      |> Enum.reduce({%{}, []}, fn
        _attr, {:_, pkey_names} ->
          {:_, pkey_names}

        attr, {pkey_match, pkey_names} ->
          with {:ok, field_filter} <- Keyword.fetch(filter, attr),
               {:ok, value} <- Keyword.fetch(field_filter, :equal) do
            {Map.put(pkey_match, attr, value), [attr | pkey_names]}
          else
            :error ->
              {:_, [attr | pkey_names]}
          end
      end)

    starting_matchspec = {{pkey_match, %{__struct__: resource}}, [], [:"$_"]}

    filter
    |> Kernel.||([])
    |> Keyword.drop(pkey_names)
    |> Enum.flat_map(fn {field, filter} ->
      Enum.map(filter, fn {type, value} ->
        {field, type, value}
      end)
    end)
    |> Enum.reduce({:ok, {starting_matchspec, %{}}}, fn
      {key, type, value}, {:ok, {spec, bindings}} ->
        do_filter_to_matchspec(resource, key, type, value, spec, bindings)

      _, {:error, error} ->
        {:error, error}
    end)
    |> case do
      {:error, error} -> {:error, error}
      {:ok, {spec, _}} -> {:ok, spec}
    end
  end

  defp do_filter_to_matchspec(resource, key, type, value, spec, binding) do
    cond do
      attr = Ash.attribute(resource, key) ->
        do_filter_to_matchspec_attribute(resource, attr.name, type, value, spec, binding)

      _rel = Ash.relationship(resource, key) ->
        {:error, "relationship filtering not supported"}

      true ->
        {:error, "unsupported filter"}
    end
  end

  defp do_filter_to_matchspec_attribute(
         _resource,
         name,
         type,
         value,
         {{id_match, struct_match}, conditions, matcher},
         bindings
       ) do
    case Map.get(bindings, name) do
      nil ->
        binding = bindings |> Map.values() |> Enum.max(fn -> 0 end) |> Kernel.+(1)
        condition = condition(type, value, binding)

        new_spec =
          {{id_match, Map.put(struct_match, name, :"$#{binding}")}, [condition | conditions],
           matcher}

        {:ok, {new_spec, Map.put(bindings, name, binding)}}

      binding ->
        condition = condition(type, value, binding)

        new_spec = {{id_match, struct_match}, [condition | conditions], matcher}

        {:ok, new_spec, bindings}
    end
  end

  def condition(:equal, value, binding) do
    {:==, :"$#{binding}", value}
  end

  def condition(:in, [value], binding) do
    condition(:equal, value, binding)
  end

  def condition(:in, [value1, value2], binding) do
    [{:orelse, {:"=:=", :"$#{binding}", value1}, {:"=:=", :"$#{binding}", value2}}]
  end

  def condition(:in, [value1 | rest], binding) do
    {:orelse, condition(:equal, value1, binding), condition(:in, rest, binding)}
  end

  def condition(:in, [], _binding) do
    [{:==, false, true}]
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

  defp match_limit(table, match_spec, limit, offset) do
    result =
      if limit do
        ETS.Set.select(table, [match_spec], limit + offset)
      else
        ETS.Set.select(table, [match_spec])
      end

    case result do
      {:ok, {matches, _}} -> {:ok, matches}
      {:ok, :"$end_of_table"} -> {:ok, []}
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
