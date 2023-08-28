defmodule Ash.Error.Invalid.AtomicsNotSupported do
  @moduledoc "Used when atomics for the given action type are not not supported by the data layer, but one is used."
  use Ash.Error.Exception

  def_ash_error([:resource, :action_type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "atomics_not_supported"

    def message(%{resource: resource, action_type: action_type}) do
      "The data layer for #{inspect(resource)} does not support atomics on #{action_type} actions"
    end
  end
end
