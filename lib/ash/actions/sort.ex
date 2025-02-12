defmodule Ash.Actions.Sort do
  @moduledoc false

  def process(resource, sort, _aggregates, _context) do
    process(resource, sort)
  end

  def process(_, ""), do: {:ok, []}

  def process(_, []), do: {:ok, []}

  def process(resource, sort) when is_binary(sort) do
    sort = String.split(sort, ",")
    process(resource, sort)
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

  def process(_resource, nil), do: {:ok, nil}

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
  def runtime_sort([], _empty, _), do: []
  def runtime_sort(results, empty, _) when empty in [nil, []], do: results
  def runtime_sort([single_result], _, _), do: [single_result]

  def runtime_sort(results, [{field, direction}], opts) do
    resource = get_resource(results, opts)

    results
    |> load_field(field, resource, opts)
    |> Enum.sort_by(&resolve_field(&1, field), to_sort_by_fun(direction))
  end

  def runtime_sort(results, [{field, direction} | rest], opts) do
    # we need check if the field supports simple equality, and if so then we can use
    # uniq_by
    #
    # otherwise, we need to do our own matching
    resource = get_resource(results, opts)

    results
    |> load_field(field, resource, opts)
    |> Enum.group_by(&resolve_field(&1, field))
    |> Enum.sort_by(fn {key, _value} -> key end, to_sort_by_fun(direction))
    |> Enum.flat_map(fn {_, records} ->
      runtime_sort(records, rest, Keyword.put(opts, :rekey?, false))
    end)
    |> maybe_rekey(results, resource, Keyword.get(opts, :rekey?, true))
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
    Enum.map(new_results, fn new_result ->
      Enum.find(results, fn result ->
        resource.primary_key_matches?(new_result, result)
      end)
    end)
  end

  defp maybe_rekey(new_results, _, _, _), do: new_results

  def runtime_distinct(results, sort, opts \\ [])
  def runtime_distinct([], _empty, _), do: []
  def runtime_distinct(results, empty, _) when empty in [nil, []], do: results
  def runtime_distinct([single_result], _, _), do: [single_result]

  def runtime_distinct([%resource{} | _] = results, distinct, opts) do
    # we need check if the field supports simple equality, and if so then we can use
    # uniq_by
    #
    # otherwise, we need to do our own matching
    fields = Enum.map(distinct, &elem(&1, 0))

    results
    |> load_field(fields, resource, opts)
    |> Enum.to_list()
    |> runtime_sort(distinct, opts)
    |> Enum.uniq_by(&Map.take(&1, fields))
  end

  defp load_field(records, field, resource, opts) do
    if is_nil(opts[:domain]) do
      records
    else
      records
      |> Stream.chunk_every(100)
      |> Stream.flat_map(fn batch ->
        query =
          resource
          |> Ash.Query.select([])
          |> Ash.Query.load(field)
          |> Ash.Query.set_context(%{private: %{internal?: true}})

        Ash.load!(batch, query,
          domain: opts[:domain],
          reuse_values?: true,
          authorize?: false,
          lazy?: opts[:lazy?] || false
        )
      end)
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
