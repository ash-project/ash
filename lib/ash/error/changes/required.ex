defmodule Ash.Error.Changes.Required do
  @moduledoc "Used when an attribute or relationship is required"
  use Ash.Error.Exception

  use Splode.Error, fields: [:field, :type, :resource], class: :invalid

  def message(error) do
    "#{error.type} #{error.field} is required"
  end
end
