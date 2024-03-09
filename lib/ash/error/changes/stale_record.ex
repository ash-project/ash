defmodule Ash.Error.Changes.StaleRecord do
  @moduledoc "Used when a stale record is attempted to be updated or deleted"
  use Ash.Error.Exception

  def_ash_error([:resource, :filters], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "stale_record"

    def message(error) do
      "Attempted to update stale record of #{inspect(error.resource)} with filter `#{error.filter}`"
    end
  end
end
