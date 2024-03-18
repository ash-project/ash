defmodule Ash.Error.Query.LockNotSupported do
  @moduledoc "Used when the data_layer does not support a given lock type"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :lock_type], class: :invalid

  def message(%{resource: resource, lock_type: lock_type}) do
    "Data layer for #{inspect(resource)} does not support lock: #{inspect(lock_type)}"
  end
end
