defmodule Ash.Error.Query.InvalidFilterValue do
  @moduledoc "Used when an invalid value is provided for a filter"
  use Ash.Error.Exception

  def_ash_error([:message, :value, :context], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_filter_value"

    def class(_), do: :invalid

    def message(%{value: value, message: message, context: context}) when not is_nil(context) do
      if message do
        "Invalid filter value `#{inspect(value)}` supplied in `#{inspect(context)}`: " <> message
      else
        "Invalid filter value `#{inspect(value)}` supplied in `#{inspect(context)}`."
      end
    end

    def message(%{value: value, message: message}) do
      if message do
        "Invalid filter value `#{inspect(value)}`: " <> message
      else
        "Invalid filter value `#{inspect(value)}`."
      end
    end

    def stacktrace(_), do: nil
  end
end
