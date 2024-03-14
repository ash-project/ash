defmodule Ash.Error.Framework.MustBeAtomic do
  @moduledoc "Used when an action that must be atomic cannot be done atomically"
  use Ash.Error.Exception

  use Splode.Error, fields: [:resource, :action, :reason], class: :framework

  def splode_message(error) do
    "#{inspect(error.resource)}.#{error.action} must be performed atomically, but it could not be: #{error.reason}"
  end
end
