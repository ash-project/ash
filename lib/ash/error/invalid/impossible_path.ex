defmodule Ash.Error.Invalid.ImpossiblePath do
  @moduledoc "Used when a request expresses a dependency on another request that doesn't exist"
  use Ash.Error.Exception

  def_ash_error([:impossible_path], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "impossible_path"

    def message(%{impossible_path: impossible_path}) do
      "Impossible path: #{inspect(impossible_path)} required by request."
    end
  end
end
