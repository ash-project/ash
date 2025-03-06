defmodule Ash.Error.Invalid.InvalidActionType do
  @moduledoc "Used when a callback returns an invalid type"
  use Ash.Error.Exception

  use Splode.Error, fields: [:message, :type, :expectation], class: :framework

  def message(%{type: type, expectation: expectation}) do
    "Expected action of type: #{inspect(expectation)} got #{inspect(type)}"
  end
end
