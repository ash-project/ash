defmodule Ash.Error.Invalid.NonStreamableAction do
  @moduledoc "Used when Api.stream is used with an action that does not support keyset pagination"
  use Ash.Error.Exception

  def_ash_error([:resource, :action, :for_bulk_update, :for_bulk_destroy], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "non_streamable_action"

    def message(%{for_bulk_update: action} = error) when not is_nil(action) do
      """
      You are attempting to pair read action #{error.action.name} with bulk update
      action #{action}, but #{inspect(error.resource)}.#{error.action.name} does not support streaming.

      To enable it, keyset pagination to the action #{error.action.name}:

          pagination keyset?: true, required?: false
      """
    end

    def message(%{for_bulk_destroy: action} = error) when not is_nil(action) do
      """
      You are attempting to pair read action #{error.action.name} with bulk destroy
      action #{action}, but #{inspect(error.resource)}.#{error.action.name} does not support streaming.

      To enable it, keyset pagination to the action #{error.action.name}:

          pagination keyset?: true, required?: false
      """
    end

    def message(error) do
      """
      Action #{inspect(error.resource)}.#{error.action.name} does not support streaming.

      To enable it, keyset pagination to the action #{error.action.name}:

          pagination keyset?: true, required?: false
      """
    end
  end
end
