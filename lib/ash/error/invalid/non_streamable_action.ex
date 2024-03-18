defmodule Ash.Error.Invalid.NonStreamableAction do
  @moduledoc "Used when Ash.stream is used with an action that does not support keyset pagination"
  use Ash.Error.Exception

  use Splode.Error,
    fields: [:resource, :action, :for_bulk_update, :for_bulk_destroy, types: [:keyset]],
    class: :invalid

  def message(%{for_bulk_update: action} = error) when not is_nil(action) do
    """
    You are attempting to pair read action #{error.action.name} with bulk update
    action #{action}, but #{inspect(error.resource)}.#{error.action.name} does not
    support streaming with one of #{inspect(error.types)}.

    #{how_to_enable(error)}
    """
  end

  def message(%{for_bulk_destroy: action} = error) when not is_nil(action) do
    """
    You are attempting to pair read action #{error.action.name} with bulk destroy
    action #{action}, but #{inspect(error.resource)}.#{error.action.name} does not
    support streaming with one of #{inspect(error.types)}.

    #{how_to_enable(error)}
    """
  end

  def message(error) do
    """
    Action #{inspect(error.resource)}.#{error.action.name} does not support streaming with one of #{inspect(error.types)}.

    #{how_to_enable(error)}
    """
  end

  defp how_to_enable(error) do
    """
    There are two ways to handle this.

    1.) Use the `allow_stream_with` or `stream_with` options to control what strategies are allowed.
    2.) Enable the respective required pagination type on the action #{error.action.name}, for example:

        # allow keyset
        pagination keyset?: true, required?: false

        # allow offset
        pagination offset?: true, required?: false

        # allow both
        pagination offset?: true, keyset?: true, required?: false
    """
  end
end
