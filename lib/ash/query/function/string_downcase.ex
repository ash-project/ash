defmodule Ash.Query.Function.StringDowncase do
  @moduledoc """
  Downcase a string
  """

  use Ash.Query.Function, name: :string_downcase

  def args,
    do: [
      [:string],
      [:ci_string]
    ]

  def new(args) do
    {:ok, %__MODULE__{arguments: args}}
  end

  def evaluate(%{arguments: [%Ash.CiString{string: value}]}) do
    {:known, Ash.CiString.new(String.downcase(value))}
  end

  def evaluate(%{arguments: [value]}) do
    {:known, String.downcase(value)}
  end

  def can_return_nil?(%{arguments: [string]}) do
    Ash.Expr.can_return_nil?(string)
  end
end
