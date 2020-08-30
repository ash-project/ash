defmodule Ash.Error.Query.AggregatesNotSupported do
  @moduledoc "Used when the data_layer does not support aggregates, or filtering/sorting them"
  use Ash.Error

  def_ash_error([:resource, :feature], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "aggregates_not_supported"

    def class(_), do: :invalid

    def message(%{resource: resource, feature: feature}) do
      "Data layer for #{inspect(resource)} does not support #{feature} aggregates"
    end

    def stacktrace(_), do: nil
  end
end
