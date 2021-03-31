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

  @doc """
  Passes the given keyword list to `Ash.Query.build/2` with the query being prepared.

  This allows declaring simple query modifications in-line. For more complicated query modifications,
  use a custom preparation.

  For example:

  ```elixir
  read :top_ten_songs do
    prepare build(sort: [song_rank: :desc], limit: 10)
  end
  ```
  """
  @spec build(Keyword.t()) :: {atom, Keyword.t()}
  def build(options) do
    {Ash.Resource.Preparation.Build, options: options}
  end
end
