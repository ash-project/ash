defmodule Ash.Error.Invalid.LimitRequired do
  @moduledoc "Used when no limit is provided, pagination is required, and no default page size is configured"
  use Ash.Error.Exception

  use Splode.Error, fields: [], class: :invalid

  def message(_) do
    "Limit is required"
  end
end
