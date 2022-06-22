defmodule Ash.Test do
  @moduledoc """
  Testing helpers for Ash.
  """

  @doc """
  Clears the `__metadata__` field and the underlying ecto `__meta__` field

  This allows for easier comparison of changeset/query results
  """
  def strip_metadata(structs) when is_list(structs), do: Enum.map(structs, &strip_metadata/1)

  def strip_metadata(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> strip_metadata()
    |> List.to_tuple()
  end

  def strip_metadata(%page_struct{results: results} = page)
      when page_struct in [Ash.Page.Offset, Ash.Page.Keyset] do
    %{page | results: Enum.map(results, &strip_metadata/1)}
  end

  def strip_metadata(%{__metadata__: _, __meta__: _} = struct) do
    struct = %{struct | __metadata__: %{}, __meta__: %Ecto.Schema.Metadata{}}

    struct
    |> Map.keys()
    |> Enum.reduce(struct, fn key, struct ->
      Map.update!(struct, key, &strip_metadata/1)
    end)
  end

  def strip_metadata(%{__metadata__: _} = struct) do
    struct = %{struct | __metadata__: %{}}

    struct
    |> Map.keys()
    |> Enum.reduce(struct, fn key, struct ->
      Map.update!(struct, key, &strip_metadata/1)
    end)
  end

  def strip_metadata(other), do: other
end
