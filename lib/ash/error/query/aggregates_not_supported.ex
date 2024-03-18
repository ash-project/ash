defmodule Ash.Error.Query.AggregatesNotSupported do
  @moduledoc "Used when the data_layer does not support aggregates, or filtering/sorting them"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :feature], class: :invalid

  def message(%{resource: resource, feature: feature}) do
    "Data layer for #{inspect(resource)} does not support #{feature} aggregates"
  end
end
