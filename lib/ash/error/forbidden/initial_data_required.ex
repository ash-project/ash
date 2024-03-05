defmodule Ash.Error.Forbidden.InitialDataRequired do
  @moduledoc "Used when "
  use Ash.Error.Exception

  def_ash_error([:source], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "initial_data_required"

    def message(%{source: source}) do
      "initial data is required for authorization for `#{source}`"
    end
  end
end
