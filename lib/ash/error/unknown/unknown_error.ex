defmodule Ash.Error.Unknown.UnknownError do
  @moduledoc "Used when an unknown error occurs"
  use Ash.Error.Exception

  def_ash_error([:error, :field], class: :unknown)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "unknown"

    def message(%{error: error}) do
      if is_binary(error) do
        to_string(error)
      else
        inspect(error)
      end
    end
  end
end
