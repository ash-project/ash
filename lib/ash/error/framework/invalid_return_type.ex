defmodule Ash.Error.Framework.InvalidReturnType do
  @moduledoc "Used when a callback returns an invalid type"
  use Ash.Error.Exception

  def_ash_error([:message], class: :framework)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_return_type"

    def message(%{message: message}) do
      message
    end
  end
end
