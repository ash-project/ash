defmodule Ash.Error.Invalid.TimeoutNotSupported do
  @moduledoc "Used when timeouts are not supported by the data layer, but one is set"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource], class: :invalid

  def message(%{resource: resource}) do
    "The data layer for #{inspect(resource)} does not support timeouts"
  end
end
