defmodule Ash.Error.Invalid.ResourceNotAllowed do
  @moduledoc "Used when a resource or alias is provided that cannot be used with the given api"
  use Ash.Error.Exception

  def_ash_error([:resource, :api], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "resource_not_allowed"

    def message(%{api: api, resource: resource}) do
      "Resource `#{inspect(resource)}` is not accepted by #{inspect(api)}"
    end
  end
end
