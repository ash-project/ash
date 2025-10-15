# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.ManualCreate do
  @moduledoc """
  A module to implement manual create actions.
  """

  defmodule Context do
    @moduledoc "The context passed into manual create action functions"

    defstruct [
      :actor,
      :select,
      :tenant,
      :tracer,
      :authorize?,
      :domain,
      :upsert?,
      :upsert_keys,
      :identity,
      :upsert_fields,
      :return_notifications?,
      source_context: %{}
    ]

    @type t :: %__MODULE__{
            actor: any(),
            select: list(atom),
            tenant: any(),
            source_context: map(),
            tracer: list(module),
            authorize?: boolean(),
            domain: Ash.Domain.t(),
            identity: Ash.Resource.Identity.t() | nil,
            upsert?: boolean(),
            upsert_keys: list(atom),
            upsert_fields: list(atom),
            return_notifications?: boolean()
          }
  end

  defmodule BulkContext do
    @moduledoc "The context passed into manual bulk_create action functions"

    defstruct [
      :actor,
      :select,
      :tenant,
      :tracer,
      :authorize?,
      :domain,
      :upsert?,
      :upsert_keys,
      :identity,
      :upsert_fields,
      :batch_size,
      :return_errors?,
      :return_notifications?,
      :return_records?,
      source_context: %{}
    ]

    @type t :: %__MODULE__{
            actor: any(),
            select: list(atom),
            tenant: any(),
            source_context: map(),
            tracer: list(module),
            authorize?: boolean(),
            domain: Ash.Domain.t(),
            identity: Ash.Resource.Identity.t() | nil,
            upsert?: boolean(),
            upsert_keys: list(atom),
            upsert_fields: list(atom),
            return_records?: boolean(),
            return_notifications?: boolean(),
            return_errors?: boolean(),
            batch_size: pos_integer()
          }
  end

  @callback create(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
              | {:error, term}

  @callback bulk_create(
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

  @optional_callbacks [bulk_create: 3]

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualCreate
    end
  end
end
