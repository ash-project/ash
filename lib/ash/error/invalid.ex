defmodule Ash.Error.Invalid do
  @moduledoc "The top level invalid error"
  use Ash.Error.Exception

  def_ash_error([:errors, stacktraces?: true], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "invalid"

    def message(%{errors: errors, stacktraces?: stacktraces?, error_context: error_context}) do
      messages = Ash.Error.error_messages(errors, nil, stacktraces?)

      case Ash.Error.breadcrumb(error_context) do
        "" ->
          messages

        error_context ->
          error_context <> "\n" <> messages
      end
    end
  end
end
