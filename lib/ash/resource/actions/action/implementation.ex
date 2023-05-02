defmodule Ash.Resource.Actions.Implementation do
  @moduledoc """
  An implementation of a basic action.
  """
  @type context :: %{
          optional(:actor) => term,
          optional(:tenant) => term,
          optional(:authorize?) => boolean,
          optional(:api) => module,
          optional(any) => any
        }

  @callback run(Ash.ActionInput.t(), opts :: Keyword.t(), context) ::
              {:ok, term()} | {:ok, [Ash.Notifier.Notification.t()]} | {:error, term()}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.Actions.Implementation
    end
  end
end
