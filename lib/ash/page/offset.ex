# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Page.Offset do
  @moduledoc """
  A page of results from `offset` based pagination.

  If a resource supports `keyset` pagination as well,
  it will also have the `keyset` metadata.
  """

  @derive {Inspect, only: [:results, :count, :limit, :offset, :more?]}
  defstruct [:results, :limit, :offset, :count, :rerun, :more?]

  @type t :: %__MODULE__{
          results: [Ash.Resource.record()],
          limit: integer(),
          count: integer(),
          offset: integer(),
          more?: boolean(),
          rerun: {Ash.Query.t(), Keyword.t()}
        }

  @type page_opts_type :: :non_neg_integer | :pos_integer | :any | :boolean
  @type page_opts_opts :: [type: page_opts_type(), doc: String.t()]
  @type page_opts :: [
          offset: page_opts_opts(),
          limit: page_opts_opts(),
          filter: page_opts_opts(),
          count: page_opts_opts()
        ]

  @page_opts [
    offset: [
      type: :non_neg_integer,
      doc: "The number of records to skip from the beginning of the query"
    ],
    limit: [
      type: :pos_integer,
      doc: "The number of records to include in the page"
    ],
    filter: [
      type: :any,
      doc: """
      A filter to apply for pagination purposes, that should not be considered in the full count.

      This is used by the liveview paginator to only fetch the records that were *already* on the
      page when refreshing data, to avoid pages jittering.
      """
    ],
    count: [
      type: :boolean,
      doc: "Whether or not to return the page with a full count of all records"
    ]
  ]

  page_opts = @page_opts

  defmodule Opts do
    @moduledoc false

    use Spark.Options.Validator, schema: page_opts
  end

  @doc false
  @spec page_opts() :: page_opts()
  def page_opts do
    @page_opts
  end

  @doc """
  Creates a new `Ash.Page.Offset.t()`.
  """
  @spec new([Ash.Resource.record()], non_neg_integer(), Ash.Query.t(), boolean(), Keyword.t()) ::
          t()
  def new(results, count, original_query, more?, opts) do
    %__MODULE__{
      results: results,
      limit: original_query.page[:limit],
      count: count,
      offset: original_query.page[:offset] || 0,
      more?: more?,
      rerun: {original_query, opts}
    }
  end
end
