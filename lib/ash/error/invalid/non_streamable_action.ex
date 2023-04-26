defmodule Ash.Error.Invalid.NonStreamableAction do
  @moduledoc "Used when Api.stream is used with an action that does not support keyset pagination"
  use Ash.Error.Exception

  def_ash_error([:resource, :action], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "non_streamable_action"

    def message(error) do
      """
      Action #{inspect(error.resource)}.#{error.action.name} does not support streaming.

      To enable it, keyset pagination to the action #{error.action.name}:

          pagination keyset?: true
      """
    end
  end
end
