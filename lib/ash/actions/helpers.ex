defmodule Ash.Actions.Helpers do
  @moduledoc false

  def select({:ok, results}, query) do
    {:ok, select(results, query)}
  end

  def select({:error, error}, _query) do
    {:error, error}
  end

  def select(results, query) when is_list(results) do
    Enum.map(results, &select(&1, query))
  end

  def select(nil, _), do: nil

  def select(result, %{select: nil}) do
    result
  end

  def select(result, %{resource: resource, select: select}) do
    resource
    |> Ash.Resource.Info.attributes()
    |> Enum.flat_map(fn attribute ->
      if attribute.private? || attribute.primary_key? || attribute.name in select do
        []
      else
        [attribute.name]
      end
    end)
    |> Enum.reduce(result, fn key, record ->
      Map.put(record, key, nil)
    end)
    |> Ash.Resource.Info.put_metadata(:selected, select)
  end
end
