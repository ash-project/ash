defmodule Ash.Query.Function.Error do
  @moduledoc """
  If the predicate is truthy, the provided exception is raised with the provided values.

  This exception is not "raised" in the Elixir sense, but the entire expression fails to
  evaluate with the given error. Various data layers will handle this differently.
  """
  use Ash.Query.Function, name: :error, eager_evaluate?: false

  def args, do: [[:atom, :any]]

  def returns, do: :no_return

  def evaluate(%{arguments: [exception, input]}) do
    json_module = Ash.Helpers.json_module()

    {:error,
     Ash.Error.from_json(exception, json_module.decode!(json_module.encode!(Map.new(input))))}
  end
end
