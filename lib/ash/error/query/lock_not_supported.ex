defmodule Ash.Error.Query.LockNotSupported do
  @moduledoc "Used when the data_layer does not support a given lock type"
  use Ash.Error.Exception

  def_ash_error([:resource, :lock_type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "lock_not_supported"

    def message(%{resource: resource, lock_type: lock_type}) do
      "Data layer for #{inspect(resource)} does not support lock: #{inspect(lock_type)}"
    end
  end
end
