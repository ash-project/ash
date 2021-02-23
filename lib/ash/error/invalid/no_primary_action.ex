defmodule Ash.Error.Invalid.NoPrimaryAction do
  @moduledoc "Used when an action name is provided that doesn't exist"
  use Ash.Error.Exception

  def_ash_error([:resource, :type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_primary_action"

    def message(%{resource: resource, type: type}) do
      "No primary action of type #{inspect(type)} for resource #{inspect(resource)}, and no action specified"
    end
  end
end
