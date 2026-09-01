defmodule Ash.Test.Manifest.Todo.DateGrouping do
  @moduledoc """
  Enum used only as a calculation argument type, pinning reachability
  discovery of calculation arguments.
  """
  use Ash.Type.Enum, values: [:day, :week, :month]
end
