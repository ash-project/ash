# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

  @doc """
  Recalculates the status of a bulk result based on its records and error_count.

  - `:success` - when there are no errors (error_count == 0)
  - `:error` - when there are only errors (records is empty or nil, error_count > 0)
  - `:partial_success` - when there are both successful records and errors
  """
  @spec recalculate_status(t()) :: t()
  def recalculate_status(%__MODULE__{error_count: 0} = result) do
    %{result | status: :success}
  end

  def recalculate_status(%__MODULE__{records: records, error_count: error_count} = result)
      when is_list(records) and records != [] and error_count > 0 do
    %{result | status: :partial_success}
  end

  def recalculate_status(%__MODULE__{error_count: error_count} = result)
      when error_count > 0 do
    %{result | status: :error}
  end

  def recalculate_status(result), do: result
end
