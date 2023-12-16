defmodule Ash.Query.Function.StringLength do
  @moduledoc """
  Trims whitespace from a string
  """

  use Ash.Query.Function, name: :string_length

  def args,
    do: [
      [:string]
    ]

  def evaluate(%{arguments: [value]}) do
    {:known, String.length(value)}
  end
end
