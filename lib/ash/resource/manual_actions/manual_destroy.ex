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

  @doc false
  @spec destroy(module(), Ash.Changeset.t() | term(), Keyword.t() | term(), Context.t() | term()) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
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
            | {:ok, Ash.Resource.record()}
            | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
            | {:ok, Ash.Resource.record(), [Ash.Notifier.Notification.t()]}
            | {:error, Ash.Error.t()}
            | {:notifications, list(Ash.Notifier.Notification.t())}
          )
  def bulk_destroy(module, changesets, opts, context) do
    result = module.bulk_destroy(changesets, opts, context)

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

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualDestroy
    end
  end
end
