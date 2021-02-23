defmodule Ash.Error.Framework.AssumptionFailed do
  @moduledoc "Used when unreachable code/conditions are reached in the framework"
  use Ash.Error.Exception

  def_ash_error([:message], class: :framework)

  defimpl Ash.ErrorKind do
    def id(_), do: Ecto.UUID.generate()

    def code(_), do: "assumption_failed"

    def message(%{message: message}) do
      """
      Assumption failed: #{message}

      This should not be possible, please report a detailed bug at:

      https://github.com/ash-project/ash/issues/new?assignees=&labels=bug%2C+needs+review&template=bug_report.md&title=
      """
    end
  end
end
