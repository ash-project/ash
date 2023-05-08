defmodule Ash.Resource.Change.Increment do
  @moduledoc """
  Increments an attribute's value by the amount specified, which defaults to 1.
  """
  use Ash.Resource.Change

  @rollover_limit 2_147_483_647

  def change(changeset, opts, _context) do
    Ash.Changeset.before_action(changeset, fn changeset ->
      value =
        changeset.data
        |> Map.get(opts[:attribute])
        |> Kernel.+(opts[:amount])
        |> overflow(opts[:amount], opts[:overflow?])

      Ash.Changeset.force_change_attribute(changeset, opts[:attribute], value)
    end)
  end

  defp overflow(value, _amount, false), do: value
  defp overflow(value, amount, true) when value >= @rollover_limit, do: amount
  defp overflow(value, _amount, true), do: value
end
