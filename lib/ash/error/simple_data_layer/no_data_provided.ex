defmodule Ash.Error.SimpleDataLayer.NoDataProvided do
  @moduledoc "Used when no data was provided to the simple data layer"
  use Ash.Error.Exception

  def_ash_error([:resource, :message], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "no_data_provided"

    def message(%{message: message}) when message not in ["", nil], do: message

    def message(%{resource: resource}) do
      "No data provided in resource #{inspect(resource)}"
    end
  end
end
