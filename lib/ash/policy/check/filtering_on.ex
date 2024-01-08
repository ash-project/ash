defmodule Ash.Policy.Check.FilteringOn do
  @moduledoc false
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "filtering on #{opts[:attribute]}"
  end

  @impl true
  def match?(
        _actor,
        %{
          query: %Ash.Query{context: %{filter_only?: true, filter_references: references}}
        },
        opts
      ) do
    path = opts[:path] || []
    field = opts[:field] || raise "Must provide field to #{inspect(__MODULE__)}"

    references
    |> Enum.filter(&(&1.relationship_path == path))
    |> Enum.any?(fn ref ->
      Ash.Query.Ref.name(ref) == field
    end)
  end

  def match?(_actor, %{query: %Ash.Query{} = query}, opts) do
    path = opts[:path] || []
    field = opts[:field] || raise "Must provide field to #{inspect(__MODULE__)}"

    query.filter
    |> Ash.Filter.list_refs()
    |> Enum.filter(&(&1.relationship_path == path))
    |> Enum.any?(fn ref ->
      Ash.Query.Ref.name(ref) == field
    end)
  end

  def match?(_, _, _), do: false
end
