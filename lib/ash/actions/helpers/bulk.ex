# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Helpers.Bulk do
  @moduledoc false

  @doc """
  Conditionally rolls back transaction for bulk operations.

  Only rolls back if `opts[:transaction]` and `opts[:rollback_on_error?]` are set
  and we're inside a transaction. Returns the error unchanged for piping.
  """
  @spec maybe_rollback(error, Ash.Resource.t(), Keyword.t()) :: error when error: term()
  def maybe_rollback(error, resource, opts) do
    if opts[:transaction] && opts[:rollback_on_error?] do
      if Ash.DataLayer.in_transaction?(resource) do
        Ash.DataLayer.rollback(resource, error)
      end
    end

    error
  end

  @doc """
  Conditionally stops bulk operation on error.

  Throws if `stop_on_error?` is set and `return_stream?` is false.
  Otherwise returns the error unchanged for piping.
  """
  @spec maybe_stop_on_error(error, Keyword.t()) :: error | no_return() when error: term()
  def maybe_stop_on_error(error, opts) do
    if opts[:stop_on_error?] && !opts[:return_stream?] do
      throw({:error, Ash.Error.to_error_class(error)})
    end

    error
  end
end
