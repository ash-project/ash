defmodule Ash.Error.Invalid do
  @moduledoc "The top level invalid error"
  use Ash.Error.Exception

  def_ash_error([:errors, :stacktraces?], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "invalid"

    def message(%{errors: errors, stacktraces?: stacktraces?}) do
      Ash.Error.error_messages(errors, nil, stacktraces?)
    end
  end
end
