defmodule Ash.Error.Invalid.Unavailable do
  @moduledoc """
  Used when a given resource is unavailable.

  This might happen due to locking at the data layer, or something
  you implement yourself.
  """
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :source, :reason], class: :invalid

  def message(%{resource: resource, source: source, reason: reason})
      when not is_nil(source) do
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
