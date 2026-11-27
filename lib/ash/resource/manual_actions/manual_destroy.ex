# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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

  @doc """
  Destroys one record using the prepared changeset.

  The record to destroy is in `changeset.data`. Return that record on success.
  Ash calls this after before-action hooks and before after-action hooks.
  Return `{:ok, record}`, `{:ok, record, notifications}` to include
  notifications, or `{:error, error}`. `opts` comes from the resource's
  `manual {module, opts}` declaration.
  """
  @callback destroy(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, Ash.Resource.Record.t()}
              | {:ok, Ash.Resource.Record.t(), list(Ash.Notifier.Notification.t())}
              | {:error, term}

  @doc """
  Destroys a batch of records when Ash invokes the manual bulk implementation.

  Return a list of results and notifications. Include successful records
  as `{:ok, record}` when the caller requests records. Use `{:error, error}`
  for a failed operation and `{:notifications, notifications}` for
  batch-level notifications. `context` is the action's `BulkContext`
  with the return flags and batch size.

  This callback is optional. Implement `c:destroy/3` for calls that operate
  on one record at a time.
  """
  @callback bulk_destroy(
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

  @optional_callbacks [bulk_destroy: 3]

  @doc false
  @spec destroy(module(), Ash.Changeset.t() | term(), Keyword.t() | term(), Context.t() | term()) ::
          {:ok, Ash.Resource.Record.t()}
          | {:ok, Ash.Resource.Record.t(), list(Ash.Notifier.Notification.t())}
          | {:error, term()}
  def destroy(module, changeset, opts, context) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :destroy,
      [changeset, opts, context],
      [{:ok, :_}, {:ok, :_, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "destroy/3"
    )
  end

  @doc false
  @spec bulk_destroy(module(), Enumerable.t(Ash.Changeset.t()), Keyword.t(), BulkContext.t()) ::
          list(
            :ok
            | {:ok, Ash.Resource.Record.t()}
            | {:ok, Ash.Resource.Record.t(), %{notifications: [Ash.Notifier.Notification.t()]}}
            | {:ok, Ash.Resource.Record.t(), [Ash.Notifier.Notification.t()]}
            | {:error, Ash.Error.t()}
            | {:notifications, list(Ash.Notifier.Notification.t())}
          )
  def bulk_destroy(module, changesets, opts, context) do
    result = apply(module, :bulk_destroy, [changesets, opts, context])

    if is_list(result) and Enum.all?(result, &valid_bulk_destroy_result?/1) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.bulk_destroy/3.

        The callback #{inspect(__MODULE__)}.bulk_destroy/3 expects a list of :ok, {:ok, record}, {:ok, record, notifications}, {:error, error}, or {:notifications, list}.
        """
    end
  end

  defp valid_bulk_destroy_result?(:ok), do: true
  defp valid_bulk_destroy_result?({:ok, _}), do: true
  defp valid_bulk_destroy_result?({:ok, _, _}), do: true
  defp valid_bulk_destroy_result?({:error, _}), do: true
  defp valid_bulk_destroy_result?({:notifications, list}) when is_list(list), do: true
  defp valid_bulk_destroy_result?(_), do: false

  @doc """
  Declares the module as a manual destroy action implementation.

  Implement `c:destroy/3`, then add `manual MyApp.CustomDestroy` to the action
  in the resource. To pass options, use `manual {MyApp.CustomDestroy, opts}`.
  The `use` macro takes no configuration options.

  See [Manual actions](/documentation/topics/actions/manual-actions.md).
  """
  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualDestroy
    end
  end
end
