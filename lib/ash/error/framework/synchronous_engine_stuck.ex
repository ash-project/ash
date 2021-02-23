defmodule Ash.Error.Framework.SynchronousEngineStuck do
  @moduledoc "Used when the sycnrhonous engine cannot proceed"
  use Ash.Error.Exception

  def_ash_error([], class: :framework)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "synchronous_engine_stuck"

    def message(_) do
      """
      Synchronous Engine Stuck

      This should not be possible, please report a detailed bug at:

      https://github.com/ash-project/ash/issues/new?assignees=&labels=bug%2C+needs+review&template=bug_report.md&title=
      """
    end
  end
end
