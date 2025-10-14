# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Sort do
  @moduledoc false

  def process(resource, sort, _aggregates, _context) do
    process(resource, sort)
  end

  def process(_, ""), do: {:ok, []}

  def process(_, []), do: {:ok, []}

  def process(resource, sort) when is_binary(sort) do
    sort = String.split(sort, ",", trim: true)
    process(resource, sort)
  end

  def process(_resource, nil), do: {:ok, nil}

  def process(resource, sort) when is_atom(sort) do
    process(resource, [sort])
  end

  def process(resource, sort) when is_list(sort) do
    sort
    |> Enum.reduce_while({:ok, []}, fn field, {:ok, sort} ->
      case Ash.Sort.parse_sort(resource, field, nil, false) do
        {:ok, value} -> {:cont, {:ok, [value | sort]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      {:error, error} -> {:error, error}
    end
  end

  def process(resource, %Ash.Query.Calculation{} = calculation) do
    process(resource, [calculation])
  end

  def process(resource, {field, order}) do
    process(resource, [{field, order}])
  end

  def sorting_on_identity?(%{sort: nil}), do: false

  def sorting_on_identity?(query) do
    identity_keys =
      query.resource
      |> Ash.Resource.Info.identities()
      |> Enum.map(& &1.keys)

    count_of_sort = Enum.count(query.sort)

    Enum.any?([Ash.Resource.Info.primary_key(query.resource) | identity_keys], fn keyset ->
      last_n_fields = query.sort |> Enum.reverse() |> Enum.take(count_of_sort)

      Enum.all?(keyset, fn key ->
        Enum.any?(last_n_fields, fn
          {sort, _} when is_atom(sort) ->
            sort == key

          _ ->
            false
        end)
      end)
    end)
  end

  @doc """
  Sort records at runtime

  Opts

  * `:domain` - The domain to use if data needs to be loaded
  * `:lazy?` - Whether to use already loaded values or to re-load them when necessary. Defaults to `false`
  * `:resource` - The resource being sorted.
  """
  def runtime_sort(results, sort, opts \\ [])

  def runtime_sort([], _sort, _opts) do
    []
  end

  def runtime_sort(results, sort, opts) do
    resource = get_resource(results, opts)

    sort =
      if Keyword.get(opts, :rename_calcs?, true) do
        sort
        |> Enum.with_index()
        |> Enum.map(fn
          {{%dynamic{load: nil} = field, order}, index}
          when dynamic in [Ash.Query.Calculation, Ash.Query.Aggregate] ->
            {%{field | name: {:__ash_runtime_sort__, index}}, order}

          {other, _} ->
            other
        end)
      else
        sort
      end

    results
    |> do_runtime_sort(resource, sort, opts)
    |> maybe_rekey(results, resource, Keyword.get(opts, :rekey?, true))
  end

  defp do_runtime_sort([], _resource, _empty, _), do: []
  defp do_runtime_sort(results, _resource, empty, _) when empty in [nil, []], do: results
  defp do_runtime_sort([single_result], _resource, _, _), do: [single_result]

  defp do_runtime_sort(results, resource, [{field, direction}], opts) do
    results
    |> load_field(field, resource, opts)
    |> Enum.sort_by(&resolve_field(&1, field), to_sort_by_fun(direction))
  end

  defp do_runtime_sort(results, resource, [{field, direction} | rest], opts) do
    results
    |> load_field(field, resource, opts)
    |> Enum.group_by(&resolve_field(&1, field))
    |> Enum.sort_by(fn {key, _value} -> key end, to_sort_by_fun(direction))
    |> Enum.flat_map(fn {_, records} ->
      do_runtime_sort(records, resource, rest, Keyword.put(opts, :rekey?, false))
    end)
  end

  defp get_resource(results, opts) do
    case opts[:resource] do
      nil ->
        case results do
          [%resource{} | _] ->
            resource

          _other ->
            raise ArgumentError,
                  "Resource must be provided when sorting a value that is not a simple list of records"
        end

      resource ->
        resource
    end
  end

  defp maybe_rekey(new_results, results, resource, true) do
    if Ash.Resource.Info.primary_key(resource) == [] do
      new_results
    else
      Enum.map(new_results, fn new_result ->
        Enum.find(results, new_result, fn result ->
          resource.primary_key_matches?(new_result, result)
        end)
      end)
    end
  end

  defp maybe_rekey(new_results, _, _, _), do: new_results

  def runtime_distinct(results, sort, opts \\ [])

  def runtime_distinct([], _sort, _opts) do
    []
  end

  def runtime_distinct(results, sort, opts) do
    resource = get_resource(results, opts)

    results
    |> do_runtime_distinct(resource, sort, opts)
    |> maybe_rekey(results, resource, Keyword.get(opts, :rekey?, true))
  end

  defp do_runtime_distinct([], _resource, _empty, _), do: []
  defp do_runtime_distinct(results, _resource, empty, _) when empty in [nil, []], do: results
  defp do_runtime_distinct([single_result], _resource, _, _), do: [single_result]

  defp do_runtime_distinct(results, resource, distinct, opts) do
    distinct =
      distinct
      |> Enum.with_index()
      |> Enum.map(fn
        {{%dynamic{load: nil} = field, order}, index}
        when dynamic in [Ash.Query.Calculation, Ash.Query.Aggregate] ->
          {%{field | name: {:__ash_runtime_sort__, index}}, order}

        {other, _} ->
          other
      end)

    fields = Enum.map(distinct, &elem(&1, 0))

    results
    |> Stream.with_index()
    |> Stream.map(fn {record, index} ->
      Ash.Resource.set_metadata(record, %{__runtime_distinct_index__: index})
    end)
    |> do_runtime_sort(
      resource,
      distinct,
      Keyword.merge(opts, rename_calcs?: false, resource: resource)
    )
    |> Stream.uniq_by(fn record ->
      Enum.map(fields, fn field ->
        resolve_field(record, field)
      end)
    end)
    |> Enum.sort_by(& &1.__metadata__.__runtime_distinct_index__)
  end

  defp load_field(records, field, resource, opts) do
    query =
      resource
      |> Ash.Query.select([])
      |> Ash.Query.load(field)
      |> Ash.Query.set_context(%{private: %{internal?: true}})

    if is_nil(opts[:domain]) do
      records
    else
      if opts[:maybe_not_distinct?] do
        Enum.map(records, fn record ->
          Ash.load!(record, query,
            domain: opts[:domain],
            reuse_values?: true,
            authorize?: false,
            lazy?: opts[:lazy?] || false
          )
        end)
      else
        records
        |> Stream.chunk_every(100)
        |> Stream.flat_map(fn batch ->
          Ash.load!(batch, query,
            domain: opts[:domain],
            reuse_values?: true,
            authorize?: false,
            lazy?: opts[:lazy?] || false
          )
        end)
      end
    end
  end

  defp resolve_field(record, %{__struct__: struct} = agg)
       when struct in [Ash.Query.Calculation, Ash.Query.Aggregate] do
    if agg.load do
      Map.get(record, agg.load)
    else
      if struct == Ash.Query.Calculation do
        Map.get(record.calculations, agg.name)
      else
        Map.get(record.aggregates, agg.name)
      end
    end
  end

  defp resolve_field(record, field) do
    record
    |> Map.get(field)
    |> case do
      %Ash.ForbiddenField{} -> nil
      other -> other
    end
  end

  defp to_sort_by_fun(:desc) do
    to_sort_by_fun(:desc_nils_first)
  end

  defp to_sort_by_fun(:asc) do
    to_sort_by_fun(:asc_nils_last)
  end

  defp to_sort_by_fun(:asc_nils_last) do
    fn x, y ->
      cond do
        is_nil(x) and is_nil(y) ->
          true

        is_nil(x) ->
          false

        is_nil(y) ->
          true

        true ->
          Comp.less_or_equal?(x, y)
      end
    end
  end

  defp to_sort_by_fun(:desc_nils_last) do
    fn x, y ->
      cond do
        is_nil(x) and is_nil(y) ->
          true

        is_nil(x) ->
          false

        is_nil(y) ->
          true

        true ->
          Comp.greater_or_equal?(x, y)
      end
    end
  end

  defp to_sort_by_fun(:asc_nils_first) do
    fn x, y ->
      cond do
        is_nil(x) and is_nil(y) ->
          true

        is_nil(x) ->
          true

        is_nil(y) ->
          false

        true ->
          Comp.less_or_equal?(x, y)
      end
    end
  end

  defp to_sort_by_fun(:desc_nils_first) do
    fn x, y ->
      cond do
        is_nil(x) and is_nil(y) ->
          true

        is_nil(x) ->
          true

        is_nil(y) ->
          false

        true ->
          Comp.greater_or_equal?(x, y)
      end
    end
  end
end
