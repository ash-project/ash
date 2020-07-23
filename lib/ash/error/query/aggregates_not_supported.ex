defmodule Ash.Error.Query.AggregatesNotSupported do
  @moduledoc "Used when the datalayer does not support filtering of aggregates"
  use Ash.Error

  def_ash_error([:resource, :predicate, :type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "aggregates_not_supported"

    def class(_), do: :invalid

    def message(%{resource: resource}) do
      "Data layer for #{inspect(resource)} does not support filtering aggregates"
    end

    def stacktrace(_), do: nil
  end
end
