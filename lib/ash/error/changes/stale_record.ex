defmodule Ash.Error.Changes.StaleRecord do
  @moduledoc "Used when a stale record is attempted to be updated or deleted"
  use Ash.Error.Exception

  def_ash_error([:resource, :filters], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "stale_record"

    def message(error) do
      filter =
        Enum.map_join(error.filters, ", ", fn {key, value} ->
          "#{key}: #{inspect(value)}"
        end)

      "record of #{inspect(error.resource)} with filter #{filter}"
    end
  end
end
