defmodule Ash.Error.Forbidden do
  @moduledoc "Used when authorization for an action fails"

  use Ash.Error

  def_ash_error([:errors, :stacktraces?], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def message(%{errors: errors, stacktraces?: stacktraces?}) when not is_nil(errors) do
      Ash.Error.error_messages(errors, nil, stacktraces?)
    end

    def message(%{errors: errors}) do
      Ash.Error.error_descriptions(errors)
    end

    def code(_), do: "Forbidden"
  end
end
