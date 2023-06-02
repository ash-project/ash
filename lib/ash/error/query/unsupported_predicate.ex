defmodule Ash.Error.Query.UnsupportedPredicate do
  @moduledoc "Used when the data_layer does not support a provided predicate"
  use Ash.Error.Exception

  def_ash_error([:resource, :predicate, :type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "unsupported_predicate"

    def message(%{resource: resource, predicate: predicate, type: type}) do
      "Data layer for #{inspect(resource)} does not support #{inspect(predicate)} for #{inspect(type)}"
    end
  end
end
