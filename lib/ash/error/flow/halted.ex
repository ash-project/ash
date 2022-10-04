defmodule Ash.Error.Flow.Halted do
  @moduledoc "Used when a flow has been halted for some reason"
  use Ash.Error.Exception

  def_ash_error([:step, :reason], class: :invalid)

  defimpl Ash.ErrorKind do
    def id(_), do: Ash.UUID.generate()

    def code(_), do: "flow_halted"

    def message(%{step: step, reason: reason}) do
      "Flow halted at #{inspect(step)}: #{inspect(reason)}"
    end
  end
end
