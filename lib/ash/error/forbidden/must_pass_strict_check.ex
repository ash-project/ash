defmodule Ash.Error.Forbidden.MustPassStrictCheck do
  @moduledoc "Used when unreachable code/conditions are reached in the framework"
  use Ash.Error

  def_ash_error([:resource], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "must_pass_strict_check"

    def message(%{resource: resource}) do
      "A request against #{inspect(resource)} was required to pass strict check, but it did not"
    end
  end
end
