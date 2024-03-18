defmodule Ash.Error.Invalid.Timeout do
  @moduledoc "Used when a request to a domain times out."
  use Ash.Error.Exception

  use Splode.Error, fields: [:name, :timeout], class: :invalid

  def message(%{name: name, timeout: timeout}) do
    """
    #{name} timed out after #{timeout}ms.

    The default timeout can be configured on the domain,

        execution do
          timeout :timer.seconds(60)
        end

    Each request can be configured with a timeout via `Ash.Changeset.timeout/2` or `Ash.Query.timeout/2`.
    """
  end
end
