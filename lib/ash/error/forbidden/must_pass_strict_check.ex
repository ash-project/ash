defmodule Ash.Error.Forbidden.MustPassStrictCheck do
  @moduledoc "Used when unreachable code/conditions are reached in the framework"
  use Ash.Error.Exception

  def_ash_error([], class: :forbidden)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "must_pass_strict_check"

    def message(_) do
      "The request was required to pass strict check, but it did not"
    end
  end
end
