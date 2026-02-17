defmodule Decimal.Error do
  @moduledoc """
  The exception that all decimal operations may raise.

  ## Fields

    * `signal` - the signalled error, additional signalled errors will be found
      in the context.
    * `reason` - the reason for the error.

  Rescuing the error to access the result or the other fields of the error is
  discouraged and should only be done for exceptional conditions. It is more
  pragmatic to set the appropriate traps on the context and check the flags
  after the operation if the result needs to be inspected.
  """

  defexception [:signal, :reason]

  @impl true
  def message(%{signal: signal, reason: reason}) do
    reason = reason && ": " <> reason
    "#{signal}#{reason}"
  end
end
