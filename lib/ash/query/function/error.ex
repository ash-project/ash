defmodule Ash.Query.Function.Error do
  @moduledoc """
  If the predicate is truthy, the provided exception is raised with the provided values.
  """
  use Ash.Query.Function, name: :raise_error

  def args, do: [[:module, :any]]

  def evaluate(%{arguments: [exception, input]}) do
    {:error, exception.exception(input)}
  end
end
