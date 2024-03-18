defmodule Ash.Error.Query.InvalidLoad do
  @moduledoc "Used when an invalid load is provided"
  use Ash.Error.Exception

  use Splode.Error, fields: [:load], class: :invalid

  def message(%{load: load}) do
    "#{inspect(load)} is not a valid load"
  end
end
