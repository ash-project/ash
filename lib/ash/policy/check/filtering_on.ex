defmodule Ash.Policy.Check.FilteringOn do
  @moduledoc "This check is true when the field provided is being referenced anywhere in a filter statement."
  use Ash.Policy.SimpleCheck

  @impl true
  def describe(opts) do
    "filtering on #{opts[:attribute]}"
  end

  @impl true
  def requires_original_data?(_, _), do: false

  @impl true
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
