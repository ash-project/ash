defmodule Ash.Page.Keyset do
  @moduledoc """
  A page of results from `keyset` based pagination.

  The results are generated with a `keyset` metadata,
  which can be used to fetch the next/previous pages.

  ## Important

  Keyset pagination cannot currently be used in conjunction with aggregate and calculation sorting.
  Combining them will result in an error on the query.
  """
  defstruct [:results, :count, :before, :after, :limit, :rerun, :more?]

  @type t :: %__MODULE__{}

  require Ash.Query

  def new(results, count, _sort, original_query, more?, opts) do
    results =
      if opts[:page][:before] do
        Enum.reverse(results)
      else
        results
      end

    %__MODULE__{
      results: data_with_keyset(results, original_query.resource),
      count: count,
      before: opts[:page][:before],
      after: opts[:page][:after],
      limit: opts[:page][:limit],
      more?: more?,
      rerun: {original_query, opts}
    }
  end

  def data_with_keyset(results, resource) do
    fields_in_keyset = Ash.Resource.Info.primary_key(resource)

    Enum.map(results, fn result ->
      Map.update!(
        result,
        :__metadata__,
        &Map.put(&1, :keyset, keyset(result, fields_in_keyset))
      )
    end)
  end

  def filter(api, resource, keyset, sort, after_or_before, query_opts)
      when after_or_before in [:after, :before] do
    sort_fields = Keyword.keys(sort)

    with {:ok, pkey_values} <- decode_values(keyset, resource, after_or_before),
         {:ok, values} <- get_values(pkey_values, api, resource, sort, query_opts),
         {:ok, zipped} <-
           zip_fields(sort_fields, values) do
      field_values =
        Enum.map(sort, fn
          {field, {_, direction}} ->
            {field, direction, Keyword.get(zipped, field)}

          {field, direction} ->
            {field, direction, Keyword.get(zipped, field)}
        end)

      {:ok, filters(field_values, after_or_before)}
    end
  end

  defp get_values(pkey, api, resource, sort, query_opts) do
    primary_read = Ash.Resource.Info.primary_action!(resource, :read)

    {select, load} =
      Enum.split_with(sort, fn {key, _order} ->
        Ash.Resource.Info.attribute(resource, key)
      end)

    select = Keyword.keys(select)

    load =
      Enum.map(load, fn
        {key, {_, input}} ->
          {key, input}

        {key, _} ->
          key
      end)

    resource
    |> Ash.Query.for_read(primary_read.name, %{}, query_opts)
    |> Ash.Query.filter(^pkey)
    |> Ash.Query.load(load)
    |> Ash.Query.select(select)
    |> Ash.Query.limit(1)
    |> api.read_one()
    |> case do
      {:ok, record} ->
        {:ok,
         Enum.map(sort, fn {key, _} ->
           Map.get(record, key)
         end)}

      other ->
        other
    end
  end

  defp decode_values(values, resource, key) do
    pkey = Ash.Resource.Info.primary_key(resource)

    {:ok,
     values
     |> URI.decode_www_form()
     |> Base.decode64!()
     |> non_executable_binary_to_term([:safe])
     |> Enum.zip_with(pkey, &{&2, &1})}
  rescue
    _e ->
      {:error, Ash.Error.Page.InvalidKeyset.exception(value: values, key: key)}
  end

  defp filters(keyset, after_or_before) do
    [or: do_filters(keyset, after_or_before)]
  end

  defp do_filters([], _), do: []

  defp do_filters([{field, direction, value} | rest], after_or_before) do
    operator = operator(after_or_before, direction)

    # keyset pagination is done like so
    # (x > a) OR
    # (x = a AND y > b) OR
    # (x = a AND y = b AND z > c) OR

    [[{field, [{operator, value}]}]] ++
      Enum.map(do_filters(rest, after_or_before), fn nested ->
        [[{field, [eq: value]}]] ++ nested
      end)
  end

  defp operator(:after, :asc), do: :gt
  defp operator(:after, :asc_nils_first), do: :gt
  defp operator(:after, :asc_nils_last), do: :gt
  defp operator(:after, :desc), do: :lt
  defp operator(:after, :desc_nulls_first), do: :lt
  defp operator(:after, :desc_nulls_last), do: :lt
  defp operator(:before, :asc), do: :lt
  defp operator(:before, :asc_nils_first), do: :lt
  defp operator(:before, :asc_nils_last), do: :lt
  defp operator(:before, :desc), do: :gt
  defp operator(:before, :desc_nulls_first), do: :gt
  defp operator(:before, :desc_nulls_last), do: :gt

  defp zip_fields(pkey, values, acc \\ [])
  defp zip_fields([], [], acc), do: {:ok, Enum.reverse(acc)}

  defp zip_fields([pkey | rest_pkey], [value | rest_values], acc) do
    zip_fields(rest_pkey, rest_values, [{pkey, value} | acc])
  end

  defp zip_fields(_, _, _), do: {:error, "Invalid keyset"}

  defp keyset(record, fields) do
    record
    |> field_values(fields)
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  defp field_values(record, fields) do
    Enum.map(fields, &Map.get(record, &1))
  end

  @doc """
  A restricted version of `:erlang.binary_to_term/2` that forbids
  *executable* terms, such as anonymous functions.
  The `opts` are given to the underlying `:erlang.binary_to_term/2`
  call, with an empty list as a default.
  By default this function does not restrict atoms, as an atom
  interned in one node may not yet have been interned on another
  (except for releases, which preload all code).
  If you want to avoid atoms from being created, then you can pass
  `[:safe]` as options, as that will also enable the safety mechanisms
  from `:erlang.binary_to_term/2` itself.
  Ripped from https://github.com/elixir-plug/plug_crypto/blob/v1.2.0/lib/plug/crypto.ex
  """
  defdelegate non_executable_binary_to_term(binary, opts), to: Ash.Helpers
end
