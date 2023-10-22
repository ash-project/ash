defmodule Ash.Error.Forbidden do
  @moduledoc "Used when authorization for an action fails"

  use Ash.Error.Exception

  def_ash_error([:errors, stacktraces?: true], class: :forbidden)

  @type t :: %__MODULE__{}

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def message(%{errors: errors, stacktraces?: stacktraces?}) when not is_nil(errors) do
      Ash.Error.error_messages(errors, nil, stacktraces?)
    end

    def message(%{errors: errors}) do
      Ash.Error.error_descriptions(errors)
    end

    def code(_), do: "forbidden"
  end
end
