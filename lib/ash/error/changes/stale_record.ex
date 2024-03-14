defmodule Ash.Error.Changes.StaleRecord do
  @moduledoc "Used when a stale record is attempted to be updated or deleted"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :filters], class: :invalid

  def splode_message(error) do
    "Attempted to update stale record of #{inspect(error.resource)} with filter `#{error.filter}`"
  end
end
