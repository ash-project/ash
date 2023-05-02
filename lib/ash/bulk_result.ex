defmodule Ash.BulkResult do
  @moduledoc """
  The return value for bulk actions.
  """

  @type t :: %__MODULE__{
          status: :success | :partial_success | :error,
          notifications: list(Ash.Notifier.Notification.t()) | nil,
          records: list(Ash.Resource.record()) | nil,
          errors: list(term) | nil
        }

  defstruct [
    :status,
    :errors,
    :records,
    :notifications
  ]
end
