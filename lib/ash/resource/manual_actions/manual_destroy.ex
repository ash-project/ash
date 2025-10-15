# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.ManualDestroy do
  @moduledoc """
  A module to implement manual destroy actions.

  Note that in the returns of these functions you must return the destroyed record or records.
  """

  defmodule Context do
    @moduledoc "The context passed into manual update action functions"

    defstruct [
      :actor,
      :tenant,
      :select,
      :tracer,
      :authorize?,
      :domain,
      :return_notifications?,
      :return_destroyed?,
      source_context: %{}
    ]

    @type t :: %__MODULE__{
            actor: any(),
            select: list(atom),
            tenant: any(),
            tracer: list(module),
            source_context: %{},
            authorize?: boolean(),
            domain: Ash.Domain.t(),
            return_notifications?: boolean(),
            return_destroyed?: boolean()
          }
  end

  defmodule BulkContext do
    @moduledoc "The context passed into manual bulk_update action functions"

    defstruct [
      :actor,
      :tenant,
      :select,
      :tracer,
      :authorize?,
      :domain,
      :return_records?,
      :return_notifications?,
      :return_errors?,
      :batch_size,
      source_context: %{}
    ]

    @type t :: %__MODULE__{
            actor: any(),
            select: list(atom),
            tenant: any(),
            tracer: list(module),
            source_context: map(),
            authorize?: boolean(),
            domain: Ash.Domain.t(),
            return_records?: boolean(),
            return_notifications?: boolean(),
            return_errors?: boolean(),
            batch_size: pos_integer()
          }
  end

  @callback destroy(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

  @callback bulk_destroy(
              changesets :: Enumerable.t(Ash.Changeset.t()),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              list(
                :ok
                | {:ok, Ash.Resource.record()}
                | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
                | {:ok, Ash.Resource.record(), [Ash.Notifier.Notification.t()]}
                | {:error, Ash.Error.t()}
                | {:notifications, list(Ash.Notifier.Notification.t())}
              )

  @optional_callbacks [bulk_destroy: 3]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualDestroy
    end
  end
end
