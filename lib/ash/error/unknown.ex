defmodule Ash.Error.Unknown do
  @moduledoc "The top level unknown error container"
  use Ash.Error.Exception

  def_ash_error([:errors, :error, :stacktraces?, :field], class: :unknown)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "unknown"

    def message(%{errors: [], error: nil} = error) do
      message(%{error | errors: [], error: "Something went wrong"})
    end

    def message(%{errors: errors, error: error, path: path, stacktraces?: stacktraces?}) do
      errors = List.wrap(errors)

      custom_prefix =
        if path && path != [] do
          inspect(path) <> " - "
        else
          ""
        end

      custom_message =
        error
        |> List.wrap()
        |> Enum.map(fn message ->
          custom_prefix <> inspect(message)
        end)

      Ash.Error.error_messages(errors, custom_message, stacktraces?)
    end
  end
end
