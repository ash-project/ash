defmodule Ash.Error.Framework.MustBeAtomic do
  @moduledoc "Used when an action that must be atomic cannot be done atomically"
  use Ash.Error.Exception

  def_ash_error([:resource, :action, :reason], class: :framework)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "must_be_atomic"

    def message(error) do
      "#{inspect(error.resource)}.#{error.action} must be performed atomically, but it could not be: #{error.reason}"
    end
  end
end
