defmodule Ash.Resource.Calculation.LoadRelationship do
  @moduledoc """
  Loads a relationship as a calculation.

  Can be used to load the same relationship with a different query.
  """
  use Ash.Calculation

  def load(query, opts, _) do
    relationship = Ash.Resource.Info.relationship(query.resource, opts[:relationship])

    query =
      opts[:query] ||
        query.resource
        |> Ash.Resource.Info.relationship(opts[:relationship])
        |> Map.get(:destination)
        |> Ash.Query.new()

    query = Ash.Query.to_query(query)

    [{relationship.name, query}]
  end

  def calculate(results, opts, _context) do
    {:ok,
     Enum.map(results, fn record ->
       Map.get(record, opts[:relationship])
     end)}
  end
end
