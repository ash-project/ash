defmodule Ash.Policy.SimpleCheck do
  @moduledoc """
  A type of check that operates only on request context, never on the data

  Define `c:match?/3`, which gets the actor, request context, and opts, and returns true or false
  """
  @type actor :: Ash.Policy.Check.actor()
  @type context :: Ash.Policy.Authorizer.t()
  @type options :: Keyword.t()

  @doc "Whether or not the request matches the check"
  @callback match?(actor(), context(), options()) :: boolean | {:ok, boolean} | {:error, term}

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Policy.SimpleCheck
      @behaviour Ash.Policy.Check

      def type, do: :simple

      def requires_original_data?(_, _), do: false

      @dialyzer {:nowarn_function, strict_check: 3}
      def strict_check(actor, context, opts) do
        case match?(actor, context, opts) do
          {:ok, value} -> {:ok, value}
          {:error, error} -> {:error, error}
          value -> {:ok, value}
        end
      end

      def prefer_expanded_description?, do: false
      def eager_evaluate?, do: true

      defoverridable requires_original_data?: 2,
                     prefer_expanded_description?: 0,
                     eager_evaluate?: 0
    end
  end
end
