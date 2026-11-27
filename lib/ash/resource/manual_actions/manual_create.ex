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

  @doc """
  Creates one record using the prepared changeset.

  Use the changeset's cast attributes and arguments to build the new record.
  Ash calls this after before-action hooks and before after-action hooks.
  Return `{:ok, record}`, `{:ok, record, %{notifications: notifications}}` to include
  notifications, or `{:error, error}`. `opts` comes from the resource's
  `manual {module, opts}` declaration.
  """
  @callback create(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, Ash.Resource.Record.t()}
              | {:ok, Ash.Resource.Record.t(), %{notifications: [Ash.Notifier.Notification.t()]}}
              | {:error, term}

  @doc """
  Creates a batch of records when Ash invokes the manual bulk implementation.

  Return a list of results and notifications. Include successful records
  as `{:ok, record}` when the caller requests records. Use `{:error, error}`
  for a failed operation and `{:notifications, notifications}` for
  batch-level notifications. `context` is the action's `BulkContext`
  with the return flags and batch size.

  This callback is optional. Implement `c:create/3` for calls that operate
  on one record at a time.
  """
  @callback bulk_create(
              changesets :: Enumerable.t(Ash.Changeset.t()),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              list(
                :ok
                | {:ok, Ash.Resource.Record.t()}
                | {:ok, Ash.Resource.Record.t(),
                   %{notifications: [Ash.Notifier.Notification.t()]}}
                | {:ok, Ash.Resource.Record.t(), [Ash.Notifier.Notification.t()]}
                | {:error, Ash.Error.t()}
                | {:notifications, list(Ash.Notifier.Notification.t())}
              )

  @optional_callbacks [bulk_create: 3]

  @doc false
  @spec create(module(), Ash.Changeset.t() | term(), Keyword.t() | term(), Context.t() | term()) ::
          {:ok, Ash.Resource.Record.t()}
          | {:ok, Ash.Resource.Record.t(), %{notifications: [Ash.Notifier.Notification.t()]}}
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
            | {:ok, Ash.Resource.Record.t()}
            | {:ok, Ash.Resource.Record.t(), %{notifications: [Ash.Notifier.Notification.t()]}}
            | {:ok, Ash.Resource.Record.t(), [Ash.Notifier.Notification.t()]}
            | {:error, Ash.Error.t()}
            | {:notifications, list(Ash.Notifier.Notification.t())}
          )
  def bulk_create(module, changesets, opts, context) do
    result = apply(module, :bulk_create, [changesets, opts, context])

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

  @doc """
  Declares the module as a manual create action implementation.

  Implement `c:create/3`, then add `manual MyApp.CustomCreate` to the action
  in the resource. To pass options, use `manual {MyApp.CustomCreate, opts}`.
  The `use` macro takes no configuration options.

  See [Manual actions](/documentation/topics/actions/manual-actions.md).
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualCreate
    end
  end
end
