defmodule Ash.Error.Invalid.ImpossiblePath do
  @moduledoc "Used when a request expresses a dependency on another request that doesn't exist"
  use Ash.Error.Exception

  def_ash_error([:impossible_path, :paths], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "impossible_path"

    def message(%{impossible_path: impossible_path, paths: nil}) do
      "Impossible path: #{inspect(impossible_path)} required by request."
    end

    def message(%{impossible_path: impossible_path, paths: paths}) do
      """
      Impossible path: #{inspect(impossible_path)} required by request.

      Available paths:
      #{Enum.map_join(paths, "\n", &inspect/1)}
      """
    end
  end
end
