defmodule Ash.Error.SideLoad.ViolatedComplexity do
  @moduledoc "Used when side loads do not satisfy complexity rules"
  use Ash.Error

  def_ash_error([:resource, :relationship], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "violated_complexity"

    def class(_), do: :invalid

    def message(%{resource: resource, relationship: relationship}) do
      "Including #{inspect(resource)}.#{relationship} violates complexity rules"
    end

    def stacktrace(_), do: nil
  end
end
