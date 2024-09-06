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

  @doc ~S"""
  Directly attach a `before_action` function to the query.

  This function will be called by `Ash.Query.before_action/2`,
  with an additional `context` argument.

  ## Example

      prepare before_action(fn query, _context ->
        Logger.debug("About to execute query for #{query.action.name} on #{inspect(query.resource)}")

        query
      end)
  """
  defmacro before_action(callback) do
    {value, function} =
      Spark.CodeHelpers.lift_functions(callback, :query_before_action, __CALLER__)

    quote generated: true do
      unquote(function)

      {Ash.Resource.Preparation.BeforeAction, callback: unquote(value)}
    end
  end

  @doc ~S"""
  Directly attach an `after_action` function to the query.

  This function will be called by `Ash.Query.after_action/2`,
  with an additional `context` argument.

  ## Example

      prepare after_action(fn query, records, _context ->
        Logger.debug("Query for #{query.action.name} on resource #{inspect(query.resource)} returned #{length(records)} records")

        {:ok, records}
      end)
  """
  defmacro after_action(callback) do
    {value, function} =
      Spark.CodeHelpers.lift_functions(callback, :query_after_action, __CALLER__)

    quote generated: true do
      unquote(function)

      {Ash.Resource.Preparation.AfterAction, callback: unquote(value)}
    end
  end
end
