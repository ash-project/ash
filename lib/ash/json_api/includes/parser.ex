defmodule Ash.JsonApi.Includes.Parser do
  # This module will have to get *WAY* hairier to support the `filter[included][foo.bar]`
  # but for now that complexity is not necessary

  defstruct [:allowed, :disallowed]

  def parse_and_validate_includes(resource, %{"include" => include_string}) do
    allowed = allowed_preloads(resource)

    include_string
    |> String.split(",")
    |> Stream.map(&String.split(&1, "."))
    |> Stream.map(fn include ->
      {include, get_in(allowed, include) || false}
    end)
    |> Enum.reduce(%__MODULE__{allowed: [], disallowed: []}, fn {include, allowed?}, acc ->
      if allowed? do
        Map.update!(acc, :allowed, fn list -> [include | list] end)
      else
        Map.update!(acc, :disallowed, fn list -> [include | list] end)
      end
    end)
  end

  def parse_and_validate_includes(_, _), do: %__MODULE__{allowed: [], disallowed: []}

  defp allowed_preloads(resource) do
    resource
    |> Ash.relationships()
    |> Enum.filter(& &1.side_load)
    |> Enum.into(%{}, fn relationship ->
      {to_string(relationship.name), to_nested_map(relationship.side_load)}
    end)
  end

  defp to_nested_map(list) when is_list(list) do
    list
    |> Enum.map(fn
      {key, value} -> {to_string(key), to_nested_map(value)}
      value -> {to_string(value), to_nested_map(value)}
    end)
    |> Enum.into(%{})
  end

  defp to_nested_map(true), do: true
  defp to_nested_map(value), do: %{value => true}
end
