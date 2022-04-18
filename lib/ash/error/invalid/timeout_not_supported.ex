defmodule Ash.Error.Invalid.TimeoutNotSupported do
  @moduledoc "Used when timeouts are not supported by the data layer, but one is set"
  use Ash.Error.Exception

  def_ash_error([:resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "timeout_not_supported"

    def message(%{resource: resource}) do
      "The data layer for #{inspect(resource)} does not support timeouts"
    end
  end
end
