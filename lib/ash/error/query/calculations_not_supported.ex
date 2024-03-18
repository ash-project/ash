defmodule Ash.Error.Query.CalculationsNotSupported do
  @moduledoc "Used when the data_layer does not support calculations, or filtering/sorting them"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :feature], class: :invalid

  def message(%{resource: resource, feature: feature}) do
    "Data layer for #{inspect(resource)} does not support #{feature} calculations"
  end
end
