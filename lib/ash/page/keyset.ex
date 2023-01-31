defmodule Ash.Page.Keyset do
  @moduledoc """
  A page of results from `keyset` based pagination.

  The results are generated with a `keyset` metadata,
  which can be used to fetch the next/previous pages.
  """
  defstruct [:results, :count, :before, :after, :limit, :rerun, :more?]

  @type t :: %__MODULE__{}

  def new(results, count, _sort, original_query, more?, opts) do
    %__MODULE__{
      results: results,
      count: count,
      before: opts[:page][:before],
      after: opts[:page][:after],
      limit: opts[:page][:limit],
      more?: more?,
      rerun: {original_query, opts}
    }
  end

  def data_with_keyset(results, _resource, sort) when is_list(results) do
    Enum.map(results, fn result ->
      Map.update!(
        result,
        :__metadata__,
        &Map.put(&1, :keyset, keyset(result, sort))
      )
    end)
  end

  def filter(resource, values, sort, after_or_before) when after_or_before in [:after, :before] do
    with {:ok, decoded} <- decode_values(values, after_or_before),
         {:ok, zipped} <- zip_fields(sort, decoded) do
      {:ok, filters(zipped, resource, after_or_before)}
    end
  end

  defp decode_values(values, key) do
    {:ok,
     values
     |> Base.decode64!()
     |> non_executable_binary_to_term([:safe])}
  rescue
    _e ->
      {:error, Ash.Error.Page.InvalidKeyset.exception(value: values, key: key)}
  end

  defp filters(keyset, resource, after_or_before) do
    [or: do_filters(keyset, resource, after_or_before)]
  end

  defp do_filters([], _, _), do: []

  defp do_filters([{field, direction, value} | rest], resource, after_or_before) do
    operator = operator(after_or_before, direction)

    # keyset pagination is done like so
    # (x > a) OR
    # (x = a AND y > b) OR
    # (x = a AND y = b AND z > c) OR

    field =
      case field do
        %Ash.Query.Calculation{} = calc ->
          %Ash.Query.Ref{attribute: calc, resource: resource, relationship_path: []}

        field ->
          field
      end

    [[{field, [{operator, value}]}]] ++
      Enum.map(do_filters(rest, resource, after_or_before), fn nested ->
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

  defp zip_fields([{pkey, direction} | rest_pkey], [value | rest_values], acc) do
    zip_fields(rest_pkey, rest_values, [{pkey, direction, value} | acc])
  end

  defp zip_fields(_, _, _), do: {:error, "Invalid keyset"}

  defp keyset(record, fields) do
    record
    |> field_values(fields)
    |> :erlang.term_to_binary()
    |> Base.encode64()
  end

  defp field_values(record, sort) do
    Enum.map(sort, fn
      {%Ash.Query.Calculation{load: load, name: name}, _} ->
        if load do
          Map.get(record, load)
        else
          Map.get(record.calculations, name)
        end

      {field, _} ->
        Map.get(record, field)
    end)
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
