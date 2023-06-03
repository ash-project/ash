defmodule Ash.Error.Query.CalculationsNotSupported do
  @moduledoc "Used when the data_layer does not support calculations, or filtering/sorting them"
  use Ash.Error.Exception

  def_ash_error([:resource, :feature], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "calculations_not_supported"

    def message(%{resource: resource, feature: feature}) do
      "Data layer for #{inspect(resource)} does not support #{feature} calculations"
    end
  end
end
