defmodule Ash.Resource.Calculation.Concat do
  @moduledoc false
  use Ash.Resource.Calculation
  require Ash.Query

  @impl Ash.Resource.Calculation
  def init(opts) do
    if opts[:keys] && is_list(opts[:keys]) && Enum.all?(opts[:keys], &is_atom/1) do
      {:ok, opts}
    else
      {:error, "Expected a `keys` option for which keys to concat"}
    end
  end

  @impl Ash.Resource.Calculation
  def load(_query, opts, _) do
    opts[:keys]
  end

  @impl Ash.Resource.Calculation
  def expression(opts, _) do
    Enum.reduce(opts[:keys], nil, fn key, expr ->
      if expr do
        if opts[:separator] do
          expr(^expr <> ^opts[:separator] <> ^ref(key))
        else
          expr(^expr <> ^ref(key))
        end
      else
        expr(^ref(key))
      end
    end)
  end

  @impl Ash.Resource.Calculation
  def calculate(records, opts, _) do
    Enum.map(records, fn record ->
      Enum.map_join(opts[:keys], opts[:separator] || "", fn key ->
        to_string(Map.get(record, key))
      end)
    end)
  end
end
