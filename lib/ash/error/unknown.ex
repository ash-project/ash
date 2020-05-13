defmodule Ash.Error.Unknown do
  use Ash.Error

  def_ash_error([:errors, :error], class: :unknown)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "unknown"

    def message(%{errors: errors}) when not is_nil(errors) do
      Ash.Error.error_messages(errors)
    end

    def message(error) do
      "Something went wrong: #{inspect(error)}"
    end

    def description(%{errors: errors}) when not is_nil(errors) do
      Ash.Error.error_descriptions(errors)
    end

    def description(_), do: "Something went wrong"
  end
end
