defmodule Ash.Error.Framework.InvalidReturnType do
  @moduledoc "Used when a callback returns an invalid type"
  use Ash.Error.Exception

  use Splode.Error, fields: [:message], class: :framework

  def message(%{message: message}) do
    message
  end
end
