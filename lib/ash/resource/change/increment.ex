defmodule Ash.Resource.Change.Increment do
  @moduledoc """
  Increments an attribute's value by the amount specified, which defaults to 1.
  """
  use Ash.Resource.Change

  def change(changeset, opts, _context) do
    Ash.Changeset.before_action(changeset, fn changeset ->
      value =
        changeset.data
        |> Map.get(opts[:attribute])
        |> Kernel.+(opts[:amount])
        |> overflow(opts[:amount], opts[:overflow_limit])

      Ash.Changeset.unsafe_change_attribute(changeset, opts[:attribute], value)
    end)
  end

  defp overflow(value, _amount, nil), do: value
  defp overflow(value, amount, overflow_limit) when value >= overflow_limit, do: amount
  defp overflow(value, _amount, _), do: value
end
