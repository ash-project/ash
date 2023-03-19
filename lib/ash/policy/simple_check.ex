defmodule Ash.Policy.SimpleCheck do
  @moduledoc """
  A type of check that operates only on request context, never on the data

  Define `c:match?/3`, which gets the actor, request context, and opts, and returns true or false
  """
  @type context :: %{
          optional(:query) => Ash.Query.t(),
          optional(:changeset) => Ash.Changeset.t(),
          :action => Ash.Resource.Actions.action(),
          :resource => Ash.Resource.t(),
          :api => Ash.Api.t()
        }
  @type options :: Keyword.t()

  @doc "Whether or not the request matches the check"
  @callback match?(actor :: struct(), context(), options) :: boolean

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Policy.SimpleCheck
      @behaviour Ash.Policy.Check

      def type, do: :simple

      def strict_check(actor, context, opts) do
        {:ok, match?(actor, context, opts)}
      end
    end
  end
end
