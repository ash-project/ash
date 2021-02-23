defmodule Ash.Resource.Preparation.Builtins do
  @moduledoc "Builtin query preparations"

  @doc """
  Merges the given query context. If an MFA is provided, it will be called with the query.

  The MFA should return `{:ok, context_to_be_merged}` or `{:error, term}`
  """
  @spec set_context(map | (Ash.Query.t() -> mfa)) ::
          {atom, Keyword.t()}
  def set_context(context) do
    {Ash.Resource.Preparation.SetContext, context: context}
  end
end
