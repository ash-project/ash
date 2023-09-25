defmodule Ash.Error.Action.InvalidOptions do
  @moduledoc "Used when options validation fails for an internal function call"
  use Ash.Error.Exception

  def_ash_error([:message], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid_options"

    def message(error) do
      error.message
    end
  end
end
