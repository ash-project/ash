defmodule Ash.Error.Invalid.NoMatchingBulkStrategy do
  @moduledoc "Used when an identity name is used that does not reference identity on the resource"
  use Ash.Error.Exception

  use Splode.Error,
    fields: [
      :resource,
      :action,
      :requested_strategies,
      :not_stream_reason,
      :not_atomic_batches_reason,
      :not_atomic_reason,
      :footer
    ],
    class: :invalid

  def message(%{
        resource: resource,
        action: action,
        footer: footer,
        not_stream_reason: not_stream_reason,
        requested_strategies: requested_strategies,
        not_atomic_batches_reason: not_atomic_batches_reason,
        not_atomic_reason: not_atomic_reason
      }) do
    action =
      if is_atom(action) do
        action
      else
        action && action.name
      end

    reasons =
      [
        "Could not use `:stream`": not_stream_reason || "Not in requested strategies",
        "Could not use `:atomic_batches`":
          not_atomic_batches_reason || "Not in requested strategies",
        "Could not use `:atomic`": not_atomic_reason || "Not in requested strategies"
      ]
      |> Enum.map_join("\n", fn {reason, message} -> "#{reason}: #{message}" end)

    footer =
      if footer do
        "\n\n#{footer}"
      end

    """
    #{inspect(resource)}.#{action} had no matching bulk strategy that could be used.

    Requested strategies: #{inspect(requested_strategies)}

    #{reasons}

    #{footer}
    """
  end
end
