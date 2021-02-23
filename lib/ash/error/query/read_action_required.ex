defmodule Ash.Error.Query.ReadActionRequired do
  @moduledoc "Used when a relationship is filtered and the destination does not have a default read action"
  use Ash.Error.Exception

  def_ash_error([:resource], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "read_action_required"

    def class(_), do: :invalid

    def message(%{resource: resource}) do
      """
      A default read action is required on the destination in order to filter
      on a relationship. Destination: #{inspect(resource)}
      """
    end

    def stacktrace(_), do: nil
  end
end
