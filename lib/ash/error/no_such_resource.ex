defmodule Ash.Error.NoSuchResource do
  @moduledoc "Used when a resource or alias is provided that doesn't exist"
  use Ash.Error

  def_ash_error([:resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_resource"

    def message(%{resource: resource}) do
      "No such resource #{inspect(resource)}"
    end

    def description(%{resource: resource}) do
      "Attempted to use a non-existing resource: #{inspect(resource)}"
    end
  end
end
