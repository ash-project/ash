defmodule Ash.Error.Invalid.Timeout do
  @moduledoc "Used when a request to the api times out."
  use Ash.Error.Exception

  def_ash_error([:name, :timeout], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "timeout"

    def message(%{name: name, timeout: timeout}) do
      """
      #{name} timed out after #{timeout}ms.

      The default timeout can be configured on the api,

          execution do
            timeout :timer.seconds(60)
          end

      Each request can be configured with a timeout via `Ash.Changeset.timeout/2` or `Ash.Query.timeout/2`.
      """
    end
  end
end
