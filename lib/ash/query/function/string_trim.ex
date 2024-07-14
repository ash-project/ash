defmodule Ash.Query.Function.StringTrim do
  @moduledoc """
  Trims whitespace from a string
  """

  use Ash.Query.Function, name: :string_trim

  def args,
    do: [
      [:string],
      [:ci_string]
    ]

  def returns, do: [:string, :ci_string]

  def evaluate(%{arguments: [nil]}), do: {:known, nil}

  def evaluate(%{arguments: [%Ash.CiString{string: value}]}) do
    {:known, Ash.CiString.new(String.trim(value))}
  end

  def evaluate(%{arguments: [value]}) do
    {:known, String.trim(value)}
  end

  def can_return_nil?(%{arguments: [string]}) do
    Ash.Expr.can_return_nil?(string)
  end
end
