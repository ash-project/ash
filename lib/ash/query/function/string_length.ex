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
end
