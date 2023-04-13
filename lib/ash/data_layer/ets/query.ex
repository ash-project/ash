defmodule Ash.DataLayer.Ets.Query do
  @moduledoc """
  Stores a ETS query as it's being built.
  """
  defstruct [
    :resource,
    :filter,
    :limit,
    :sort,
    :tenant,
    :api,
    calculations: [],
    aggregates: [],
    relationships: %{},
    offset: 0
  ]
end
