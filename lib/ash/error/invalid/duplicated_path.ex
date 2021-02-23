defmodule Ash.Error.Invalid.DuplicatedPath do
  @moduledoc "Used when multiple requests with the same path are passed to the internal engine"
  use Ash.Error.Exception

  def_ash_error([:paths], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "duplicated_path"

    def message(%{paths: paths}) do
      "Duplicate requests at paths: #{inspect(paths)}"
    end
  end
end
