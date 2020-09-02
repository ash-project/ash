defmodule Ash.Resource.Calculation.Concat do
  @moduledoc "An example concatenation calculation, that accepts the delimeter as an argument"
  use Ash.Calculation, type: :string

  def init(opts) do
    if opts[:keys] && is_list(opts[:keys]) && Enum.all?(opts[:keys], &is_atom/1) do
      {:ok, opts}
    else
      {:error, "Expected a `keys` option for which keys to concat"}
    end
  end

  def calculate(records, opts, _) do
    Enum.map(records, fn record ->
      Enum.map_join(opts[:keys], opts[:separator] || "", fn key ->
        to_string(Map.get(record, key))
      end)
    end)
  end
end
