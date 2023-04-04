defmodule Ash.Error.Unknown do
  @moduledoc "The top level unknown error container"
  use Ash.Error.Exception

  def_ash_error([:errors, stacktraces?: true], class: :unknown)

  def exception(opts) do
    if opts[:error] do
      super(Keyword.update(opts, :errors, [opts[:error]], &[opts[:error] | &1]))
    else
      super(opts)
    end
  end

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "unknown"

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
