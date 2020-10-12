defmodule Ash.Page.Offset do
  @moduledoc """
  A page of results from `offset` based pagination.

  If a resource supports `keyset` pagination as well,
  it will also have the `keyset` metadata.
  """
  defstruct [:results, :limit, :offset, :count, :rerun]

  @type t :: %__MODULE__{}

  def new(results, count, original_query, opts) do
    %__MODULE__{
      results: results,
      limit: opts[:page][:limit],
      count: count,
      offset: opts[:page][:offset] || 0,
      rerun: {original_query, opts}
    }
  end
end
