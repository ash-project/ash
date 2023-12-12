defmodule Ash.Query.Function.Error do
  @moduledoc """
  If the predicate is truthy, the provided exception is raised with the provided values.
  """
  use Ash.Query.Function, name: :error, eager_evaluate?: false

  def args, do: [[:atom, :any]]

  def evaluate(%{arguments: [exception, input]}) do
    {:error, exception.exception(input)}
  end
end
