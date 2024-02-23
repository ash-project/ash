defmodule Ash.Resource.Calculation.LoadRelationship do
  @moduledoc """
  Loads a relationship as a calculation.

  Can be used to load the same relationship with a different query.
  """
  use Ash.Resource.Calculation

  def load(query, opts, _) do
    relationship = Ash.Resource.Info.relationship(query.resource, opts[:relationship])

    [relationship.source_attribute]
  end

  # We should be doing this in the load callback, not the `calculate/3` callback
  # however, we don't have much of a choice currently. We need to rewrite data loading
  # from the ground up, and a byproduct of that will be making data loading more efficient
  # across the board.
  def calculate([], _, _), do: {:ok, []}

  def calculate([%resource{} | _] = results, opts, context) do
    relationship = Ash.Resource.Info.relationship(resource, opts[:relationship])

    query =
      opts[:query] ||
        resource
        |> Ash.Resource.Info.relationship(opts[:relationship])
        |> Map.get(:destination)
        |> Ash.Query.new()

    query = Ash.Query.to_query(query)

    if !opts[:api] do
      raise "Must provide the `api` option to load #{inspect(__MODULE__)}"
    end

    load_opts = Ash.context_to_opts(context, opts[:opts] || [])

    opts[:api].load(results, [{relationship.name, query}], load_opts)
    |> case do
      {:ok, results} ->
        {:ok,
         Enum.map(results, fn result ->
           Map.get(result, relationship.name)
         end)}

      {:error, error} ->
        {:error, error}
    end
  end
end
