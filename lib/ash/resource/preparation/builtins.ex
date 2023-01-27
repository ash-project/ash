defmodule Ash.Resource.Preparation.Builtins do
  @moduledoc "Builtin query preparations"

  @doc """
  Merges the given query context.

  If an MFA is provided, it will be called with the changeset.
  The MFA should return `{:ok, context_to_be_merged}` or `{:error, term}`

  ## Examples

      change set_context(%{something_used_internally: true})
      change set_context({MyApp.Context, :set_context, []})
  """
  @spec set_context(context :: map | mfa) ::
          Ash.Resource.Preparation.ref()
  def set_context(context) do
    {Ash.Resource.Preparation.SetContext, context: context}
  end

  @doc """
  Passes the given keyword list to `Ash.Query.build/2` with the query being prepared.

  This allows declaring simple query modifications in-line.

  To see the available options, see `Ash.Query.build/2`

  ## Examples

      prepare build(sort: [song_rank: :desc], limit: 10)
      prepare build(load: [:friends])
  """

  @spec build(Keyword.t()) :: Ash.Resource.Preparation.ref()
  def build(options) do
    {Ash.Resource.Preparation.Build, options: options}
  end
end
