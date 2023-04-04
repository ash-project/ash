defmodule Ash.Error.Forbidden do
  @moduledoc "Used when authorization for an action fails"

  use Ash.Error.Exception

  def_ash_error([:errors, stacktraces?: true], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def message(%{errors: errors, stacktraces?: stacktraces?, error_context: error_context}) do
      messages = Ash.Error.error_messages(errors, nil, stacktraces?)

      case Ash.Error.breadcrumb(error_context) do
        "" ->
          messages

        error_context ->
          error_context <> "\n" <> messages
      end
    end

    def code(_), do: "Forbidden"
  end
end
