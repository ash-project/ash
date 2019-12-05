defmodule Ash.Authorization.Check.Static do
  @moduledoc """
  If this check is reached, it returns the static value provided.

  Primarily useful for testing. There is no need to end a rule chain with this.
  Instead, you can make the last rule a `deny_only`, or `allow_only` rule.
  """
  use Ash.Authorization.Check

  def init(opts) do
    case opts[:result] do
      value when is_boolean(value) ->
        {:ok, [result: value]}

      _ ->
        {:error, "`result` must be a boolean"}
    end
  end

  # in the current design this should technically not be reachable
  def check(_, _, _, opts), do: opts[:result]

  def describe(opts) do
    "the current user is the #{opts[:relationship]}"
  end

  def precheck(_, _, opts), do: {:precheck, opts[:result]}
end
