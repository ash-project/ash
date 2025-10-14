# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Page.Unpaged do
  # Utility struct to store all the info for a paged relationship to be first split between
  # related records and then paged
  @moduledoc false
  @type t :: %__MODULE__{
          related_records: Ash.Resource.record(),
          opts: Keyword.t()
        }

  defstruct [:related_records, :opts]

  @doc """
  Creates a new `Ash.Page.Unpaged.t`.
  """
  @spec new([Ash.Resource.record()], Keyword.t()) :: t()
  def new(related_records, opts) do
    %__MODULE__{
      related_records: related_records,
      opts: Keyword.delete(opts, :return_unpaged?)
    }
  end
end
