defmodule Ash.Page.Offset do
  @moduledoc """
  A page of results from `offset` based pagination.

  If a resource supports `keyset` pagination as well,
  it will also have the `keyset` metadata.
  """
  defstruct [:results, :limit, :offset, :count, :rerun, :more?]

  @type t :: %__MODULE__{}

  def new(results, count, original_query, more?, opts) do
    %__MODULE__{
      results: Enum.take(results, (original_query.limit || 1) - 1),
      limit: opts[:page][:limit],
      count: count,
      offset: opts[:page][:offset] || 0,
      more?: more?,
      rerun: {original_query, opts}
    }
  end
end
