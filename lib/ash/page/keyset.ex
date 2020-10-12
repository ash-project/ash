defmodule Ash.Page.Keyset do
  @moduledoc """
  A page of results from `keyset` based pagination.

  The results are generated with a `keyset` metadata,
  which can be used to fetch the next/previous pages.
  """
  defstruct [:results, :count, :before, :after, :limit, :rerun]

  @type t :: %__MODULE__{}

  def new(results, count, sort, original_query, opts) do
    %__MODULE__{
      results: data_with_keyset(results, sort),
      count: count,
      before: opts[:page][:before],
      after: opts[:page][:after],
      limit: opts[:page][:limit],
      rerun: {original_query, opts}
    }
  end

  def data_with_keyset(results, sort) do
    fields_in_keyset =
      sort
      |> Keyword.keys()
      |> Enum.sort()

    Enum.map(results, fn result ->
      Map.update!(
        result,
        :metadata,
        &Map.put(&1, :keyset, keyset(result, fields_in_keyset))
      )
    end)
  end

  def filter(values, sort, after_or_before) when after_or_before in [:after, :before] do
    sort_fields =
      sort
      |> Keyword.keys()
      |> Enum.sort()

    with {:ok, decoded} <- decode_values(values),
         {:ok, zipped} <- zip_fields(sort_fields, decoded) do
      field_values =
        Enum.map(sort, fn {field, direction} ->
          {field, direction, Keyword.get(zipped, field)}
        end)

      {:ok, filters(field_values, after_or_before)}
    end
  end

  defp decode_values(values) do
    {:ok,
     values
     |> Base.decode64!()
     |> non_executable_binary_to_term([:safe])}
  rescue
    e ->
      {:error, e}
  end

  defp filters([{field, direction, value} | rest], after_or_before) do
    operator = operator(after_or_before, direction)

    case rest do
      [] ->
        [{field, [{operator, value}]}]

      rest ->
        [
          and: [
            [{field, [{operator, value}]}],
            [or: [[{field, [{operator, value}]}], filters(rest, after_or_before)]]
          ]
        ]
    end
  end

  defp operator(:after, :asc), do: :gt
  defp operator(:after, :desc), do: :lt
  defp operator(:before, :asc), do: :lt
  defp operator(:before, :desc), do: :gt

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

  # A restricted version of `:erlang.binary_to_term/2` that forbids
  # *executable* terms, such as anonymous functions.
  # The `opts` are given to the underlying `:erlang.binary_to_term/2`
  # call, with an empty list as a default.
  # By default this function does not restrict atoms, as an atom
  # interned in one node may not yet have been interned on another
  # (except for releases, which preload all code).
  # If you want to avoid atoms from being created, then you can pass
  # `[:safe]` as options, as that will also enable the safety mechanisms
  # from `:erlang.binary_to_term/2` itself.
  # Ripped from https://github.com/elixir-plug/plug_crypto/blob/v1.2.0/lib/plug/crypto.ex

  # sobelow_skip ["Misc.BinToTerm"]
  defp non_executable_binary_to_term(binary, opts) when is_binary(binary) do
    term = :erlang.binary_to_term(binary, opts)
    non_executable_terms(term)
    term
  end

  defp non_executable_terms(list) when is_list(list) do
    non_executable_list(list)
  end

  defp non_executable_terms(tuple) when is_tuple(tuple) do
    non_executable_tuple(tuple, tuple_size(tuple))
  end

  defp non_executable_terms(map) when is_map(map) do
    folder = fn key, value, acc ->
      non_executable_terms(key)
      non_executable_terms(value)
      acc
    end

    :maps.fold(folder, map, map)
  end

  defp non_executable_terms(other)
       when is_atom(other) or is_number(other) or is_bitstring(other) or is_pid(other) or
              is_reference(other) do
    other
  end

  defp non_executable_terms(other) do
    raise ArgumentError,
          "cannot deserialize #{inspect(other)}, the term is not safe for deserialization"
  end

  defp non_executable_list([]), do: :ok

  defp non_executable_list([h | t]) when is_list(t) do
    non_executable_terms(h)
    non_executable_list(t)
  end

  defp non_executable_list([h | t]) do
    non_executable_terms(h)
    non_executable_terms(t)
  end

  defp non_executable_tuple(_tuple, 0), do: :ok

  defp non_executable_tuple(tuple, n) do
    non_executable_terms(:erlang.element(n, tuple))
    non_executable_tuple(tuple, n - 1)
  end
end
