defmodule Ash.Error.Framework.Unsupported do
  @moduledoc "Used when an operation is unsupported by the framework"

  use Splode.Error, fields: [:message], class: :framework

  def message(%{message: message}) do
    message
  end
end
