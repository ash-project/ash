defmodule Ash.Error.Invalid.NoSuchAction do
  @moduledoc "Used when an action name is provided that doesn't exist"
  use Ash.Error

  def_ash_error([:resource, :action, :type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "no_such_action"

    def message(%{resource: resource, action: action, type: type}) do
      "No such action #{inspect(action)} of type #{inspect(type)} for resource #{
        inspect(resource)
      }"
    end
  end
end
