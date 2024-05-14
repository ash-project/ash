defmodule Ash.Query.Function.StringLength do
  @moduledoc """
  Trims whitespace from a string
  """

  use Ash.Query.Function, name: :string_length

  def args,
    do: [
      [:string],
      [:ci_string]
    ]

  def evaluate(%{arguments: [%Ash.CiString{string: value}]}) do
    {:known, String.length(value)}
  end

  def evaluate(%{arguments: [value]}) do
    {:known, String.length(value)}
  end

  def can_return_nil?(%{arguments: [string]}) do
    Ash.Expr.can_return_nil?(string)
  end
end
