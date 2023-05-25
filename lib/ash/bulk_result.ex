defmodule Ash.BulkResult do
  @moduledoc """
  The return value for bulk actions.
  """

  @type t :: %__MODULE__{
          status: :success | :partial_success | :error,
          notifications: list(Ash.Notifier.Notification.t()) | nil,
          records: list(Ash.Resource.record()) | nil,
          errors: list(Ash.Error.t() | Ash.Changeset.t()) | nil,
          error_count: non_neg_integer()
        }

  defstruct [
    :status,
    :errors,
    :records,
    :notifications,
    error_count: 0
  ]
end
