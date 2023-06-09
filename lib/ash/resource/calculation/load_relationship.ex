defmodule Ash.Resource.Calculation.LoadRelationship do
  @moduledoc """
  Loads a relationship as a calculation.

  Can be used to load the same relationship with a different query.
  """
  use Ash.Calculation

  # TODO: This actually can be optimized to just depend on the load it needs!
  # i.e put the logic from `calculate` into `load` and then just `Map.get` the value :)

  def load(query, opts, _) do
    relationship = Ash.Resource.Info.relationship(query.resource, opts[:relationship])

    [relationship.source_attribute]
  end

  def calculate([], _, _), do: {:ok, []}

  def calculate([%resource{} | _] = list, opts, context) do
    api = opts[:api]

    query =
      opts[:query] ||
        resource
        |> Ash.Resource.Info.relationship(opts[:relationship])
        |> Map.get(:destination)
        |> Ash.Query.new()

    query = Ash.Query.to_query(query)

    load_opts =
      context
      |> Map.take([:actor, :tenant, :authorize?, :tracer])
      |> Map.to_list()
      |> Keyword.merge(opts[:opts] || [])

    list
    |> api.load([{opts[:relationship], query}], load_opts)
    |> case do
      {:ok, values} ->
        {:ok,
         Enum.map(values, fn record ->
           Map.get(record, opts[:relationship])
         end)}

      {:error, error} ->
        {:error, error}
    end
  end
end
