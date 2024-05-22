defmodule Ash.Reactor.BulkUpdateStep do
  @moduledoc """
  The Reactor stop which is used to execute create actions in bulk.
  """

  use Reactor.Step
  import Ash.Reactor.StepUtils
  alias Ash.{BulkResult, DataLayer}

  @doc false
  @impl true
  def run(arguments, context, options) do
    bulk_update_options =
      options
      |> Keyword.take([
        :allow_stream_with,
        :assume_casted?,
        :atomic_update,
        :authorize_changeset_with,
        :authorize_query_with,
        :authorize_query?,
        :authorize?,
        :batch_size,
        :domain,
        :filter,
        :load,
        :lock,
        :max_concurrency,
        :notify?,
        :read_action,
        :resource,
        :return_errors?,
        :return_notifications?,
        :return_records?,
        :return_stream?,
        :rollback_on_error?,
        :select,
        :skip_unknown_inputs,
        :sorted?,
        :stop_on_error?,
        :strategy,
        :stream_batch_size,
        :stream_with,
        :timeout,
        :transaction
      ])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])
      |> maybe_set_kw(:notification_metadata, arguments[:notification_metadata])

    success_states =
      options[:success_state]
      |> case do
        :partial_success -> [:success, :partial_success]
        _ -> [:success]
      end

    return_stream? = options[:return_stream?]

    arguments.initial
    |> Ash.bulk_update(options[:action], arguments[:input], bulk_update_options)
    |> case do
      result when is_struct(result, BulkResult) ->
        if result.status in success_states do
          maybe_queue_notifications(result, context, options[:notify?])
        else
          {:error, result}
        end

      stream when return_stream? == true ->
        {:ok, stream}
    end
  end

  @doc false
  @impl true
  def undo(bulk_result, arguments, _context, options) when is_struct(bulk_result, BulkResult) do
    action_options =
      options
      |> Keyword.take([:authorize?, :domain])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    options[:resource]
    |> Ash.ActionInput.for_action(options[:undo_action], %{bulk_result: bulk_result})
    |> Ash.run_action(action_options)
    |> case do
      :ok -> :ok
      {:ok, _} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc false
  @impl true
  def can?(%{impl: {_, options}}, :undo) do
    case options[:undo] do
      :always ->
        true

      :never ->
        false

      :outside_transaction ->
        !DataLayer.in_transaction?(options[:resource]) || options[:transaction] != false
    end
  end

  def can?(_, :compensate), do: false

  defp maybe_queue_notifications(result, _context, false), do: {:ok, result}

  defp maybe_queue_notifications(result, _context, true) when is_nil(result.notifications),
    do: {:ok, result}

  defp maybe_queue_notifications(result, _context, true) when result.notifications == [],
    do: {:ok, result}

  defp maybe_queue_notifications(result, context, true) do
    with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, result.notifications) do
      {:ok, %{result | notifications: nil}}
    end
  end
end
