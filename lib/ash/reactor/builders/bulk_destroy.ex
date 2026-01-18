# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.BulkDestroy do
  @moduledoc false

  alias Ash.Reactor.BulkDestroyStep
  alias Ash.Resource.Info
  alias Reactor.{Argument, Builder}
  alias Spark.{Dsl.Transformer, Error.DslError}
  import Ash.Reactor.BuilderUtils
  import Reactor.Template, only: :macros

  @doc false
  @impl true
  def build(bulk_destroy, reactor) do
    with {:ok, reactor, arguments} <- build_input_arguments(reactor, bulk_destroy) do
      initial = %Argument{name: :initial, source: bulk_destroy.initial}

      notification_metadata =
        case bulk_destroy.notification_metadata do
          template when is_template(template) ->
            %Argument{name: :notification_metadata, source: template}

          map when is_map(map) ->
            Argument.from_value(:notification_metadata, map)
        end

      arguments =
        arguments
        |> maybe_append(bulk_destroy.actor)
        |> maybe_append(bulk_destroy.tenant)
        |> maybe_append(bulk_destroy.load)
        |> maybe_append(bulk_destroy.context)
        |> Enum.concat(bulk_destroy.wait_for)
        |> Enum.concat([initial, notification_metadata])

      action_options =
        bulk_destroy
        |> Map.take([
          :action,
          :allow_stream_with,
          :authorize_changeset_with,
          :authorize_query_with,
          :authorize_query?,
          :authorize?,
          :batch_size,
          :domain,
          :filter,
          :lock,
          :max_concurrency,
          :notify?,
          :page,
          :read_action,
          :resource,
          :return_errors?,
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
          :success_state,
          :timeout,
          :transaction,
          :undo_action,
          :undo
        ])
        |> Map.put(:return_notifications?, bulk_destroy.notify?)
        |> Enum.reject(&is_nil(elem(&1, 1)))

      step_options =
        bulk_destroy |> Map.take([:async?]) |> Map.put(:ref, :step_name) |> Enum.to_list()

      Builder.add_step(
        reactor,
        bulk_destroy.name,
        {BulkDestroyStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def verify(bulk_destroy, dsl_state) do
    action_error_path = [:bulk_destroy, bulk_destroy.name, :action]

    with {:ok, action} <-
           get_action(dsl_state, bulk_destroy.resource, bulk_destroy.action, action_error_path),
         :ok <-
           verify_action_type(
             dsl_state,
             bulk_destroy.resource,
             action,
             :destroy,
             action_error_path
           ),
         :ok <- verify_undo(dsl_state, bulk_destroy),
         :ok <- maybe_verify_undo_action(dsl_state, bulk_destroy),
         :ok <- verify_select(dsl_state, bulk_destroy),
         :ok <- verify_rollback_on_error(dsl_state, bulk_destroy),
         :ok <- verify_sorted(dsl_state, bulk_destroy) do
      verify_notify(dsl_state, bulk_destroy)
    end
  end

  defguardp is_falsy(value) when value in [nil, false]

  defp verify_notify(dsl_state, bulk_destroy)
       when bulk_destroy.notify? == true and bulk_destroy.return_stream? == true,
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_destroy, bulk_destroy.name, :notify?],
            message: """
            Setting `notify?` has no effect when `return_stream?` is `true`.

            You must manually consume the resulting stream of records and notifications in a subsequent step.
            """
          )}

  defp verify_notify(_dsl_state, _bulk_destroy), do: :ok

  defp verify_sorted(dsl_state, bulk_destroy)
       when bulk_destroy.sorted? == true and is_falsy(bulk_destroy.return_records?),
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_destroy, bulk_destroy.name, :sorted?],
            message: "Setting `sorted?` has no effect with `return_records?` is not `true`."
          )}

  defp verify_sorted(_dsl_state, _bulk_destroy), do: :ok

  defp verify_rollback_on_error(dsl_state, bulk_destroy)
       when bulk_destroy.rollback_on_error? == true and is_falsy(bulk_destroy.transaction),
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_destroy, bulk_destroy.name, :rollback_on_error?],
            message: "Setting `rollback_on_error?` has no effect when `transaction` is `false`."
          )}

  defp verify_rollback_on_error(_dsl_state, _bulk_destroy), do: :ok

  defp verify_select(_dsl_state, bulk_destroy) when bulk_destroy.select == [], do: :ok
  defp verify_select(_dsl_state, bulk_destroy) when bulk_destroy.return_records? == true, do: :ok

  defp verify_select(dsl_state, bulk_destroy),
    do:
      {:error,
       DslError.exception(
         module: Transformer.get_persisted(dsl_state, :module),
         path: [:bulk_destroy, bulk_destroy.name, :select],
         message: "Setting `select` has no effect when `return_records?` is not `true`."
       )}

  defp verify_undo(dsl_state, bulk_destroy)
       when bulk_destroy.undo != :never and bulk_destroy.return_stream? == true,
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_destroy, bulk_destroy.name, :undo],
            message:
              "Cannot set undo to anything other than `:never` when `return_stream?` is `true`."
          )}

  defp verify_undo(_dsl_state, _bulk_destroy), do: :ok

  defp maybe_verify_undo_action(_dsl_state, bulk_destroy) when bulk_destroy.undo == :never,
    do: :ok

  defp maybe_verify_undo_action(dsl_state, bulk_destroy) do
    error_path = [:bulk_destroy, bulk_destroy.name, :undo_action]

    with {:ok, action} <-
           get_action(dsl_state, bulk_destroy.resource, bulk_destroy.undo_action, error_path),
         :ok <- verify_action_type(dsl_state, bulk_destroy.resource, action, :action, error_path) do
      verify_action_takes_bulk_result(dsl_state, bulk_destroy.resource, action, error_path)
    end
  end

  defp get_action(dsl_state, resource, action_name, error_path) do
    case Info.action(resource, action_name) do
      nil ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: error_path,
           message:
             "No action found matching the name `#{action_name}` on resource `#{inspect(resource)}`."
         )}

      action when is_struct(action) ->
        {:ok, action}
    end
  end

  defp verify_action_type(_dsl_state, _resource, action, action_type, _error_path)
       when action.type == action_type,
       do: :ok

  defp verify_action_type(dsl_state, resource, action, action_type, error_path) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: error_path,
       message:
         "Expected the action `#{inspect(action.name)}` on `#{inspect(resource)}` to be a #{action_type}, however it is a #{action.type}"
     )}
  end

  defp verify_action_takes_bulk_result(
         _dsl_state,
         _resource,
         %{arguments: [%{name: :bulk_result}]},
         _error_path
       ),
       do: :ok

  defp verify_action_takes_bulk_result(dsl_state, _resource, _action, error_path) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: error_path,
       message:
         "The undo action for a bulk destroy step should take a single `bulk_result` argument."
     )}
  end
end
