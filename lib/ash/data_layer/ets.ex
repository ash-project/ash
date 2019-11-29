defmodule Ash.DataLayer.Ets do
  @moduledoc """
  An ETS backed Ash Datalayer. Should only be used for testing, or for
  unimportant/small datasets.
  """

  @behaviour Ash.DataLayer

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @data_layer Ash.DataLayer.Ets
      @mix_ins Ash.DataLayer.Ets

      @ets_private? Keyword.get(opts, :private?, false)

      def ets_private?() do
        @ets_private?
      end
    end
  end

  def before_compile_hook(_env) do
    quote do
      struct_fields =
        @attributes
        |> Enum.map(fn attr ->
          {attr.name, nil}
        end)
        |> Enum.concat(Enum.map(@relationships, fn rel -> {rel.name, :not_loaded} end))

      defstruct struct_fields
    end
  end

  def private?(resource) do
    resource.ets_private?()
  end

  defmodule Query do
    defstruct [:resource, :filter, :limit, :sort, offset: 0]
  end

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
    # :ets.fun2ms(fn {_, })
    Enum.reduce(filter, {:ok, query}, fn
      _, {:error, error} ->
        {:error, error}

      {key, value}, {:ok, query} ->
        do_filter(query, key, value, resource)
    end)
  end

  @impl true
  def sort(query, sort, _resource) do
    {:ok, %{query | sort: sort}}
  end

  defp do_filter(query, field, id, _resource) do
    {:ok, %{query | filter: Map.put(query.filter || %{}, field, id)}}
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

  defp filter_to_matchspec(resource, filter) do
    starting_matchspec = {{:_, %{__struct__: resource}}, [], [:"$_"]}

    filter
    |> Kernel.||(%{})
    |> Enum.reduce({:ok, {starting_matchspec, 1}}, fn
      {key, value}, {:ok, {spec, binding}} ->
        do_filter_to_matchspec(resource, key, value, spec, binding)

      _, {:error, error} ->
        {:error, error}
    end)
    |> case do
      {:error, error} -> {:error, error}
      {:ok, {spec, _}} -> {:ok, spec}
    end
  end

  # TODO: Assuming id field, fix at somepoint
  defp do_filter_to_matchspec(
         _resource,
         :id,
         id,
         {{_, struct_match}, conditions, matcher},
         binding
       ) do
    condition = {:==, :"$#{binding}", id}

    {:ok, {{{:"$#{binding}", struct_match}, [condition | conditions], matcher}, binding + 1}}
  end

  defp do_filter_to_matchspec(resource, key, value, spec, binding) do
    cond do
      attr = Ash.attribute(resource, key) ->
        do_filter_to_matchspec_attribute(resource, attr, value, spec, binding)

      _rel = Ash.relationship(resource, key) ->
        {:error, "relationship filtering not supported"}

      true ->
        {:error, "unsupported filter"}
    end
  end

  defp do_filter_to_matchspec_attribute(
         _resource,
         %{name: name},
         value,
         {{id_match, struct_match}, conditions, matcher},
         binding
       ) do
    condition = {:==, :"$#{binding}", value}

    new_spec =
      {{id_match, Map.put(struct_match, name, :"$#{binding}")}, [condition | conditions], matcher}

    {:ok, {new_spec, binding + 1}}
  end

  @impl true
  def create(_resource, _attributes, relationships) when relationships != %{} do
    {:error, "#{inspect(__MODULE__)} does not support creating with relationships"}
  end

  def create(resource, attributes, _relationships) do
    with {:ok, table} <- wrap_or_create_table(resource),
         attrs <- Map.put_new_lazy(attributes, :id, &Ash.UUID.generate/0),
         record <- struct(resource, attrs),
         {:ok, _} <- Ets.Set.put(table, {attrs.id, record}) do
      {:ok, record}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp match_limit(table, match_spec, limit, offset) do
    # TODO: Fix this
    # This is a hack :(
    # Either implement cursor based pagination
    # or find a way to skip in ETS
    result =
      if limit do
        Ets.Set.select(table, [match_spec], limit + offset)
      else
        Ets.Set.select(table, [match_spec])
      end

    case result do
      {:ok, {matches, _}} -> {:ok, matches}
      {:ok, :"$end_of_table"} -> {:ok, []}
      {:error, error} -> {:error, error}
    end
  end

  defp wrap_or_create_table(resource) do
    case Ets.Set.wrap_existing(resource) do
      {:error, :table_not_found} ->
        protection =
          if private?(resource) do
            :private
          else
            :public
          end

        Ets.Set.new(
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
