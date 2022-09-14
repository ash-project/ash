defmodule Ash.Query.Function.Length do
  @moduledoc """
  Returns the length of a list attribute defined by the composite type `{:array, Type}`.

      length(roles)

  If the attribute allows nils:

      length(roles || [])

  """
  use Ash.Query.Function, name: :length

  def args, do: [[{:array, :any}]]

  def evaluate(%{arguments: [val]}) do
    {:known, Enum.count(val)}
  end
end
