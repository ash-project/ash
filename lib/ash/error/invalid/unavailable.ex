defmodule Ash.Error.Invalid.Unavailable do
  @moduledoc """
  Used when a given resource is unavailable.

  This might happen due to locking at the data layer, or something
  you implement yourself.
  """
  use Ash.Error.Exception

  def_ash_error([:resource, :source, :reason], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "unavailable"

    def message(%{resource: resource, source: source, reason: reason}) when not is_nil(source) do
      """
      #{inspect(resource)} - the resource is not available#{with_reason(reason)}.

      In:

      #{source}
      """
    end

    def message(%{resource: resource, reason: reason}),
      do: "#{inspect(resource)} - the resource is not available#{with_reason(reason)}"

    defp with_reason(nil), do: ""
    defp with_reason(reason), do: ": #{reason}"
  end
end
