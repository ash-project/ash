defmodule Ash.Resource.Change.Increment do
  @moduledoc """
  Increments an attribute's value by the amount specified, which defaults to 1.
  """
  use Ash.Resource.Change
  require Ash.Expr

  def change(changeset, opts, _context) do
    Ash.Changeset.before_action(changeset, fn changeset ->
      value =
        changeset.data
        |> Map.get(opts[:attribute])
        |> Kernel.+(opts[:amount])
        |> overflow(opts[:amount], opts[:overflow_limit])

      Ash.Changeset.force_change_attribute(changeset, opts[:attribute], value)
    end)
  end

  def atomic(changeset, opts, _context) do
    value = Ash.Changeset.atomic_ref(changeset, opts[:attribute])

    if opts[:overflow_limit] do
      {:atomic,
       %{
         opts[:attribute] =>
           Ash.Expr.expr(
             if ^value + ^opts[:amount] > ^opts[:overflow_limit] do
               ^opts[:amount]
             else
               ^value + ^opts[:amount]
             end
           )
       }}
    else
      {:atomic, %{opts[:attribute] => Ash.Expr.expr(^value + ^opts[:amount])}}
    end
  end

  defp overflow(value, _amount, nil), do: value
  defp overflow(value, amount, overflow_limit) when value > overflow_limit, do: amount
  defp overflow(value, _amount, _), do: value
end
