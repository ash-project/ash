# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.ManualUpdate do
  @moduledoc """
  A module to implement manual update actions.
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
      :return_notifications?,
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
            return_notifications?: boolean()
          }
  end

  defmodule BulkContext do
    @moduledoc "The context passed into manual update action functions"

    defstruct [
      :actor,
      :select,
      :tenant,
      :source_context,
      :tracer,
      :authorize?,
      :domain,
      :return_records?,
      :return_notifications?,
      :return_errors?,
      :batch_size
    ]

    @type t :: %__MODULE__{
            actor: any(),
            select: list(atom),
            source_context: map(),
            tenant: any(),
            tracer: list(module),
            authorize?: boolean(),
            domain: Ash.Domain.t(),
            return_records?: boolean(),
            return_notifications?: boolean(),
            return_errors?: boolean(),
            batch_size: pos_integer()
          }
  end

  @callback update(
              changeset :: Ash.Changeset.t(),
              opts :: Keyword.t(),
              context :: Context.t()
            ) ::
              {:ok, Ash.Resource.record()}
              | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
              | {:error, term}

  @callback bulk_update(
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

  @optional_callbacks [bulk_update: 3]

  @doc false
  @spec update(module(), Ash.Changeset.t() | term(), Keyword.t() | term(), Context.t() | term()) ::
          {:ok, Ash.Resource.record()}
          | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
          | {:error, term()}
  def update(module, changeset, opts, context) do
    Ash.BehaviourHelpers.call_and_validate_return(
      module,
      :update,
      [changeset, opts, context],
      [{:ok, :_}, {:ok, :_, :_}, {:error, :_}],
      behaviour: __MODULE__,
      callback_name: "update/3"
    )
  end

  @doc false
  @spec bulk_update(module(), Enumerable.t(Ash.Changeset.t()), Keyword.t(), BulkContext.t()) ::
          list(
            :ok
            | {:ok, Ash.Resource.record()}
            | {:ok, Ash.Resource.record(), %{notifications: [Ash.Notifier.Notification.t()]}}
            | {:ok, Ash.Resource.record(), [Ash.Notifier.Notification.t()]}
            | {:error, Ash.Error.t()}
            | {:notifications, list(Ash.Notifier.Notification.t())}
          )
  def bulk_update(module, changesets, opts, context) do
    result = apply(module, :bulk_update, [changesets, opts, context])

    if is_list(result) and Enum.all?(result, &valid_bulk_update_result?/1) do
      result
    else
      raise Ash.Error.Framework.InvalidReturnType,
        message: """
        Invalid value returned from #{inspect(module)}.bulk_update/3.

        The callback #{inspect(__MODULE__)}.bulk_update/3 expects a list of :ok, {:ok, record}, {:ok, record, notifications}, {:error, error}, or {:notifications, list}.
        """
    end
  end

  defp valid_bulk_update_result?(:ok), do: true
  defp valid_bulk_update_result?({:ok, _}), do: true
  defp valid_bulk_update_result?({:ok, _, _}), do: true
  defp valid_bulk_update_result?({:error, _}), do: true
  defp valid_bulk_update_result?({:notifications, list}) when is_list(list), do: true
  defp valid_bulk_update_result?(_), do: false

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Resource.ManualUpdate
    end
  end
end
