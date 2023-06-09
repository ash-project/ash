defmodule Ash.Resource.Calculation.LoadAttribute do
  @moduledoc """
  Loads an attribute as a calculation.

  Can be used to load the same attribute with different load statements applied.
  """
  use Ash.Calculation

  def load(_query, opts, _) do
    [opts[:attribute]]
  end

  def calculate(list, opts, context) do
    api = opts[:api]

    load_opts =
      context
      |> Map.take([:actor, :tenant, :authorize?, :tracer])
      |> Map.to_list()
      |> Keyword.merge(opts[:opts] || [])

    if opts[:load] do
      api.load(list, [{opts[:attribute], opts[:load]}], load_opts)
    else
      api.load(list, [opts[:attribute]], load_opts)
    end
    |> case do
      {:ok, values} ->
        {:ok,
         Enum.map(values, fn record ->
           Map.get(record, opts[:attribute])
         end)}

      {:error, error} ->
        {:error, error}
    end
  end
end
