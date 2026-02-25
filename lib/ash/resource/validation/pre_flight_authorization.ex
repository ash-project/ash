# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Validation.PreFlightAuthorization do
  @moduledoc "Validates that the action is being run in a pre-flight authorization context (i.e. `Ash.can?/3`)."
  use Ash.Resource.Validation

  @impl true
  def init(opts) do
    {:ok, opts}
  end

  @impl true
  def supports(_opts), do: [Ash.Changeset, Ash.Query, Ash.ActionInput]

  @impl true
  def validate(subject, _opts, _context) do
    if subject.context[:private][:pre_flight_authorization?] do
      :ok
    else
      {:error,
       Ash.Error.Unknown.UnknownError.exception(
         error: "action is not being run in a pre-flight authorization context"
       )}
    end
  end

  @impl true
  def atomic(subject, opts, context) do
    validate(subject, opts, context)
  end

  @impl true
  def describe(_opts) do
    [message: "must be in a pre-flight authorization context", vars: %{}]
  end
end
