defmodule Ash.Error.Query.NoSuchFunction do
  @moduledoc "Used when an function that doesn't exist is used in a query"
  use Ash.Error.Exception

  def_ash_error([:name, :arity, :resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_such_function"

    def message(error) do
      if error.arity do
        "No such function #{error.name}/#{error.arity}"
        |> for_resource(error.resource)
      else
        "No such function #{error.name}"
        |> for_resource(error.resource)
      end
    end

    defp for_resource(message, nil), do: message
    defp for_resource(message, resource), do: message <> " for resource #{inspect(resource)}"
  end
end
