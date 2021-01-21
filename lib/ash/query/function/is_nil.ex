defmodule Ash.Query.Function.IsNil do
  @moduledoc """
  true if the provided field is nil
  """
  use Ash.Query.Function, name: :is_nil

  def args, do: [:any]

  def evaluate(%{arguments: [val]}) do
    {:known, is_nil(val)}
  end
end
