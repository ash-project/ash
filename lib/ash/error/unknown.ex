defmodule Ash.Error.Unknown do
  @moduledoc "The top level unknown error container"
  use Ash.Error.Exception

  def_ash_error([:errors, :error, :field, stacktraces?: true], class: :unknown)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "unknown"

    def message(%{errors: [], error: nil} = error) do
      message(%{error | errors: [], error: "Something went wrong"})
    end

    def message(%{
          errors: errors,
          error: error,
          path: path,
          stacktraces?: stacktraces?,
          stacktrace: stacktrace
        }) do
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
          cond do
            is_binary(message) ->
              custom_prefix <> message

            is_exception(message) ->
              case message do
                %{stacktrace: %{stacktrace: stacktrace}} ->
                  custom_prefix <> Exception.format(:error, message, stacktrace)

                message ->
                  custom_prefix <> Exception.format(:error, message)
              end

            true ->
              custom_prefix <> inspect(message)
          end
        end)

      if stacktrace && stacktrace.stacktrace do
        Ash.Error.error_messages(errors, custom_message, stacktraces?) <>
          "\n" <>
          Exception.format_stacktrace(stacktrace.stacktrace)
      else
        Ash.Error.error_messages(errors, custom_message, stacktraces?)
      end
    end
  end
end
