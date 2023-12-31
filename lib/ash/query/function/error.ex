defmodule Ash.Query.Function.Error do
  @moduledoc """
  If the predicate is truthy, the provided exception is raised with the provided values.

  This exception is not "raised" in the Elixir sense, but the entire expression fails to
  evaluate with the given error. Various data layers will handle this differently.
  """
  use Ash.Query.Function, name: :error, eager_evaluate?: false

  def args, do: [[:atom, :any]]

  def evaluate(%{arguments: [exception, input]}) do
    {:error, Ash.Error.from_json(exception, Jason.decode!(Jason.encode!(Map.new(input))))}
  end
end
