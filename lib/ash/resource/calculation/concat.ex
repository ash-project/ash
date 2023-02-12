defmodule Ash.Resource.Calculation.Concat do
  @moduledoc false
  use Ash.Calculation
  require Ash.Query

  def init(opts) do
    if opts[:keys] && is_list(opts[:keys]) && Enum.all?(opts[:keys], &is_atom/1) do
      {:ok, opts}
    else
      {:error, "Expected a `keys` option for which keys to concat"}
    end
  end

  def load(_query, opts, _) do
    opts[:keys]
  end

  def expression(opts, _) do
    Enum.reduce(opts[:keys], nil, fn key, expr ->
      if expr do
        if opts[:separator] do
          Ash.Query.expr(^expr <> ^opts[:separator] <> ref(^key))
        else
          Ash.Query.expr(^expr <> ref(^key))
        end
      else
        Ash.Query.expr(ref(^key))
      end
    end)
  end

  def calculate(records, opts, _) do
    Enum.map(records, fn record ->
      Enum.map_join(opts[:keys], opts[:separator] || "", fn key ->
        to_string(Map.get(record, key))
      end)
    end)
  end
end
