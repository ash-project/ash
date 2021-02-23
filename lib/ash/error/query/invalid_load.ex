defmodule Ash.Error.Query.InvalidLoad do
  @moduledoc "Used when an invalid load is provided"
  use Ash.Error.Exception

  def_ash_error([:load], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_load"

    def message(%{load: load}) do
      "#{inspect(load)} is not a valid load"
    end
  end
end
