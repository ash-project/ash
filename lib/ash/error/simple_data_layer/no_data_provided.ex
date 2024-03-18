defmodule Ash.Error.SimpleDataLayer.NoDataProvided do
  @moduledoc "Used when no data was provided to the simple data layer"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :message], class: :framework

  def message(%{message: message}) when message not in ["", nil], do: message

  def message(%{resource: resource}) do
    "No data provided in resource #{inspect(resource)}"
  end
end
