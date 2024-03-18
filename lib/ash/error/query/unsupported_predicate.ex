defmodule Ash.Error.Query.UnsupportedPredicate do
  @moduledoc "Used when the data_layer does not support a provided predicate"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :predicate, :type], class: :invalid

  def message(%{resource: resource, predicate: predicate, type: type}) do
    "Data layer for #{inspect(resource)} does not support #{inspect(predicate)} for #{inspect(type)}"
  end
end
