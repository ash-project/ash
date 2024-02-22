defmodule Ash.Policy.SimpleCheck do
  @moduledoc """
  A type of check that operates only on request context, never on the data

  Define `c:match?/3`, which gets the actor, request context, and opts, and returns true or false
  """
  @type context :: %{
          required(:action) => Ash.Resource.Actions.action(),
          required(:resource) => Ash.Resource.t(),
          required(:api) => Ash.Api.t(),
          optional(:query) => Ash.Query.t(),
          optional(:changeset) => Ash.Changeset.t(),
          optional(any) => any
        }
  @type options :: Keyword.t()

  @doc "Whether or not the request matches the check"
  @callback match?(actor :: struct(), context(), options) :: boolean

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Policy.SimpleCheck
      @behaviour Ash.Policy.Check

      def type, do: :simple

      def requires_original_data?(_, _), do: false

      def strict_check(actor, context, opts) do
        {:ok, match?(actor, context, opts)}
      end

      defoverridable requires_original_data?: 2
    end
  end
end
