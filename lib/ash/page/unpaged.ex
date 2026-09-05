# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Page.Unpaged do
  # Utility struct to store all the info for a paged relationship to be first split between
  # related records and then paged
  @moduledoc false
  @type t :: %__MODULE__{
          related_records: Ash.Resource.Record.t(),
          opts: Keyword.t(),
          more_by_source: %{term() => boolean()}
        }

  defstruct [:related_records, :opts, more_by_source: %{}]

  @doc """
  Creates a new `Ash.Page.Unpaged.t`.

  `more_by_source` records, per `__lateral_join_source__`, whether that source
  record has a next page. The extra row pagination fetches to determine this is
  dropped before the related records get here.
  """
  @spec new([Ash.Resource.Record.t()], Keyword.t(), %{term() => boolean()}) :: t()
  def new(related_records, opts, more_by_source \\ %{}) do
    %__MODULE__{
      related_records: related_records,
      opts: Keyword.delete(opts, :return_unpaged?),
      more_by_source: more_by_source
    }
  end
end
