defmodule Ash.Resource.Calculation.LoadAttribute do
  @moduledoc """
  Loads an attribute as a calculation.

  Can be used to load the same attribute with different load statements applied.
  """
  use Ash.Resource.Calculation

  def load(_query, opts, _) do
    [opts[:attribute]]
  end

  def calculate(list, opts, context) do
    load_opts =
      Ash.Context.to_opts(context, Keyword.put(opts[:opts] || [], :domain, opts[:domain]))

    if opts[:load] do
      Ash.load(list, [{opts[:attribute], opts[:load]}], load_opts)
    else
      Ash.load(list, [opts[:attribute]], load_opts)
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
