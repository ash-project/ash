defmodule Ash.Error.Query.NoSuchFunction do
  @moduledoc "Used when an function that doesn't exist is used in a query"
  use Ash.Error.Exception

  use Splode.Error, fields: [:function, :arity, :resource], class: :invalid

  def message(error) do
    if error.arity do
      "No such function #{error.function}/#{error.arity}"
      |> for_resource(error.resource)
    else
      "No such function #{error.function}"
      |> for_resource(error.resource)
    end
  end

  defp for_resource(message, nil), do: message
  defp for_resource(message, resource), do: message <> " for resource #{inspect(resource)}"
end
