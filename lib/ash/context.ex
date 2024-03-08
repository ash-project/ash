defmodule Ash.Context do
  @moduledoc """
  Functions for working with the context provided to various callbacks in Ash.
  """

  @doc """
  Copies keys from the given context map into a keyword list. Does *not* copy the `:domain` key.

  Keys copied:

  * `:actor`
  * `:authorize?`
  * `:tracer`
  * `:tenant`
  """
  def to_opts(map, add_to \\ []) when is_map(map) do
    add_to
    |> add_if_present(map, :actor)
    |> add_if_present(map, :authorize?)
    |> add_if_present(map, :tracer)
    |> add_if_present(map, :tenant)
  end

  defp add_if_present(opts, map, key) do
    case Map.fetch(map, key) do
      {:ok, value} -> Keyword.put(opts, key, value)
      :error -> opts
    end
  end
end
