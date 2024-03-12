defmodule Ash.Resource.ManualCreate do
  @moduledoc """
  A module to implement manual create actions.
  """

  defmodule Context do
    @moduledoc "The context passed into manual update action functions"

    defstruct [
      :actor,
      :select,
      :tenant,
      :tracer,
      :authorize?,
      :domain,
      :upsert?,
      :upsert_keys,
      :upsert_fields,
      :return_records?,
      :batch_size
    ]

    @type t :: %__MODULE__{
            actor: any(),
            select: list(atom),
            tenant: any(),
            tracer: list(module),
            authorize?: boolean(),
            domain: Ash.Domain.t(),
            upsert?: boolean(),
            upsert_keys: list(atom),
            upsert_fields: list(atom),
            return_records?: boolean(),
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
