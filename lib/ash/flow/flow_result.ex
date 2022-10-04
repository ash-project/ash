defmodule Ash.Flow.Result do
  @moduledoc """
  The result of running a flow.
  """

  defstruct [
    :flow,
    :result,
    params: %{},
    input: %{},
    notifications: [],
    errors: [],
    valid?: false,
    complete?: false
  ]

  @type t :: %__MODULE__{
          flow: Ash.Flow.t(),
          result: any | nil,
          params: map(),
          input: map(),
          notifications: list(Ash.Notifier.Notification.t()),
          errors: list(Ash.Error.t()),
          valid?: boolean,
          complete?: boolean
        }
end
