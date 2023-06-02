defmodule Ash.Error.Invalid.LimitRequired do
  @moduledoc "Used when no limit is provided, pagination is required, and no default page size is configured"
  use Ash.Error.Exception

  def_ash_error([], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "limit_is_required"

    def message(_) do
      "Limit is required"
    end
  end
end
