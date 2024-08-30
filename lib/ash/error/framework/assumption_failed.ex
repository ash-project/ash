defmodule Ash.Error.Framework.AssumptionFailed do
  @moduledoc "Used when unreachable code/conditions are reached in the framework"
  use Splode.Error, fields: [:message], class: :framework

  def message(%{message: message}) do
    """
    Assumption failed: #{message}

    This should not be possible, please report a detailed bug at:

    https://github.com/ash-project/ash/issues/new?assignees=&labels=bug%2C+needs+review&template=bug_report.md&title=
    """
  end
end
