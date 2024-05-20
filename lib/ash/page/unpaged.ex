defmodule Ash.Page.Unpaged do
  # Utility struct to store all the info for a paged relationship to be first split between
  # related records and then paged
  @moduledoc false

  defstruct [:related_records, :opts]

  def new(related_records, opts) do
    %__MODULE__{
      related_records: related_records,
      opts: Keyword.delete(opts, :return_unpaged?)
    }
  end
end
