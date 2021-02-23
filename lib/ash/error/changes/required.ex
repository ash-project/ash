defmodule Ash.Error.Changes.Required do
  @moduledoc "Used when an attrbute or relationship is required"
  use Ash.Error.Exception

  def_ash_error([:field, :type], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "required"

    def message(error) do
      "#{error.type} #{error.field} is required"
    end
  end
end
