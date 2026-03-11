# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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

  @doc false
  @spec create(module(), Ash.Changeset.t() | term(), Keyword.t() | term(), Context.t() | term()) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
          | {:error, term()}
  def create(module, changeset, opts, context) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :create,
      [changeset, opts, context],
      [{:ok, :_}, {:ok, :_, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "create/3"
    )
  end

  @doc false
  @spec bulk_create(module(), Enumerable.t(Ash.Changeset.t()), Keyword.t(), BulkContext.t()) ::
          list(
            :ok
            | {:ok, Ash.Resource.record()}
            | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
            | {:ok, Ash.Resource.record(), [Ash.Notifier.Notification.t()]}
            | {:error, Ash.Error.t()}
            | {:notifications, list(Ash.Notifier.Notification.t())}
          )
  def bulk_create(module, changesets, opts, context) do
    result = module.bulk_create(changesets, opts, context)

    if is_list(result) and Enum.all?(result, &valid_bulk_create_result?/1) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.bulk_create/3.

        The callback #{inspect(__MODULE__)}.bulk_create/3 expects a list of :ok, {:ok, record}, {:ok, record, notifications}, {:error, error}, or {:notifications, list}.
        """
    end
  end

  defp valid_bulk_create_result?(:ok), do: true
  defp valid_bulk_create_result?({:ok, _}), do: true
  defp valid_bulk_create_result?({:ok, _, _}), do: true
  defp valid_bulk_create_result?({:error, _}), do: true
  defp valid_bulk_create_result?({:notifications, list}) when is_list(list), do: true
  defp valid_bulk_create_result?(_), do: false

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualCreate
    end
  end
end
