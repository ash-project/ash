defmodule Ash.Error.Framework do
  @moduledoc "Used when an unknown/generic framework error occurs"
  use Ash.Error.Exception

  def_ash_error([:errors], class: :framework)

  @type t :: %__MODULE__{}

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "framework"

    def message(%{errors: errors}) do
      Ash.Error.error_messages(errors, nil)
    end
  end
end
