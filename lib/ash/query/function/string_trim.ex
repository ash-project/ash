defmodule Ash.Query.Function.StringTrim do
  @moduledoc """
  Trims whitespace from a string
  """

  use Ash.Query.Function, name: :string_trim

  def args,
    do: [
      [:string]
    ]

  def evaluate(%{arguments: [nil]}), do: {:known, nil}

  def evaluate(%{arguments: [value]}) do
    {:known, String.trim(value)}
  end
end
