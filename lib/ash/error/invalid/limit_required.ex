defmodule Ash.Error.Invalid.LimitRequired do
  @moduledoc "Used when no limit is provided, pagination is required, and no default page size is configured"
  use Ash.Error

  def_ash_error([], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "limit_is_required"

    def class(_), do: :invalid

    def message(_) do
      "Limit is required"
    end

    def stacktrace(_), do: nil
  end
end
