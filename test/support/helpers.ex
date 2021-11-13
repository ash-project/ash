defmodule Ash.Test.Helpers do
  @moduledoc false

  def clear_meta({:ok, record}) do
    {:ok, clear_meta(record)}
  end

  def clear_meta({:error, error}), do: {:error, error}

  def clear_meta(value) when is_list(value) do
    Enum.map(value, &clear_meta/1)
  end

  def clear_meta(%Ash.Page.Offset{results: results} = page) do
    %{page | results: Enum.map(results, &clear_meta/1)}
  end

  def clear_meta(%Ash.Page.Keyset{results: results} = page) do
    %{page | results: Enum.map(results, &clear_meta/1)}
  end

  def clear_meta(%{__metadata__: _} = record) do
    Map.put(record, :__metadata__, %{})
  end

  def clear_meta(other), do: other

  defmacro hydrated_expr(resource, expr) do
    quote do
      Ash.Query.expr(unquote(expr))
      |> Ash.Filter.hydrate_refs(%{
        resource: unquote(resource),
        aggregates: %{},
        calculations: %{},
        public?: false
      })
    end
  end
end
