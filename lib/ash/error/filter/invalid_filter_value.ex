defmodule Ash.Error.Filter.InvalidFilterValue do
  @moduledoc "Used when an invalid value is provided for a filter"
  use Ash.Error

  def_ash_error([:field, :filter, :value], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "invalid_filter_value"

    def class(_), do: :invalid

    def message(%{field: field, value: value, filter: filter}) do
      "Invalid filter value #{inspect(value)} supplied for #{inspect(field)}#{inspect(filter)}"
    end

    def description(%{field: field, filter: filter, value: value}) do
      "Invalid filter value #{inspect(value)} supplied for #{inspect(field)}#{inspect(filter)}"
    end

    def stacktrace(_), do: nil
  end
end
