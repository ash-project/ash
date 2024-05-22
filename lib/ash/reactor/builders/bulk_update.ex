defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.BulkUpdate do
  @moduledoc false

  alias Ash.Reactor.BulkUpdateStep
  alias Ash.Resource.Info
  alias Reactor.{Argument, Builder}
  alias Spark.{Dsl.Transformer, Error.DslError}
  import Ash.Reactor.BuilderUtils
  import Reactor.Template, only: :macros

  @doc false
  @impl true
  def build(bulk_update, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, bulk_update) do
      initial = %Argument{name: :initial, source: bulk_update.initial}

      notification_metadata =
        case bulk_update.notification_metadata do
          template when is_template(template) ->
            %Argument{name: :notification_metadata, source: template}

          map when is_map(map) ->
            Argument.from_value(:notification_metadata, map)
        end

      arguments =
        arguments
        |> maybe_append(bulk_update.actor)
        |> maybe_append(bulk_update.tenant)
        |> Enum.concat(bulk_update.wait_for)
        |> Enum.concat([initial, notification_metadata])

      action_options =
        bulk_update
        |> Map.take([
          :action,
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
          :page,
          :read_action,
          :resource,
          :return_errors?,
          :return_records?,
          :return_stream?,
          :reuse_values?,
          :rollback_on_error?,
          :select,
          :skip_unknown_inputs,
          :sorted?,
          :stop_on_error?,
          :strategy,
          :stream_batch_size,
          :stream_with,
          :timeout,
          :transaction,
          :undo_action,
          :undo
        ])
        |> Map.put(:return_notifications?, bulk_update.notify?)
        |> Enum.reject(&is_nil(elem(&1, 1)))

      step_options =
        bulk_update |> Map.take([:async?]) |> Map.put(:ref, :step_name) |> Enum.to_list()

      Builder.add_step(
        reactor,
        bulk_update.name,
        {BulkUpdateStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def transform(_bulk_update, dsl_state), do: {:ok, dsl_state}

  @doc false
  @impl true
  def verify(bulk_update, dsl_state) do
    action_error_path = [:bulk_update, bulk_update.name, :action]

    with {:ok, action} <-
           get_action(dsl_state, bulk_update.resource, bulk_update.action, action_error_path),
         :ok <-
           verify_action_type(dsl_state, bulk_update.resource, action, :update, action_error_path),
         :ok <- verify_undo(dsl_state, bulk_update),
         :ok <- maybe_verify_undo_action(dsl_state, bulk_update),
         :ok <- verify_select(dsl_state, bulk_update),
         :ok <- verify_rollback_on_error(dsl_state, bulk_update),
         :ok <- verify_sorted(dsl_state, bulk_update) do
      verify_notify(dsl_state, bulk_update)
    end
  end

  defguardp is_falsy(value) when value in [nil, false]

  defp verify_notify(dsl_state, bulk_update)
       when bulk_update.notify? == true and bulk_update.return_stream? == true,
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_update, bulk_update.name, :notify?],
            message: """
            Setting `notify?` has no effect when `return_stream?` is `true`.

            You must manually consume the resulting stream of records and notifications in a subsequent step.
            """
          )}

  defp verify_notify(_dsl_state, _bulk_update), do: :ok

  defp verify_sorted(dsl_state, bulk_update)
       when bulk_update.sorted? == true and is_falsy(bulk_update.return_records?),
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_update, bulk_update.name, :sorted?],
            message: "Setting `sorted?` has no effect with `return_records?` is not `true`."
          )}

  defp verify_sorted(_dsl_state, _bulk_update), do: :ok

  defp verify_rollback_on_error(dsl_state, bulk_update)
       when bulk_update.rollback_on_error? == true and is_falsy(bulk_update.transaction),
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_update, bulk_update.name, :rollback_on_error?],
            message: "Setting `rollback_on_error?` has no effect when `transaction` is `false`."
          )}

  defp verify_rollback_on_error(_dsl_state, _bulk_update), do: :ok

  defp verify_select(_dsl_state, bulk_update) when bulk_update.select == [], do: :ok
  defp verify_select(_dsl_state, bulk_update) when bulk_update.return_records? == true, do: :ok

  defp verify_select(dsl_state, bulk_update),
    do:
      {:error,
       DslError.exception(
         module: Transformer.get_persisted(dsl_state, :module),
         path: [:bulk_update, bulk_update.name, :select],
         message: "Setting `select` has no effect when `return_records?` is not `true`."
       )}

  defp verify_undo(dsl_state, bulk_update)
       when bulk_update.undo != :never and bulk_update.return_stream? == true,
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_update, bulk_update.name, :undo],
            message:
              "Cannot set undo to anything other than `:never` when `return_stream?` is `true`."
          )}

  defp verify_undo(_dsl_state, _bulk_update), do: :ok

  defp maybe_verify_undo_action(_dsl_state, bulk_update) when bulk_update.undo == :never, do: :ok

  defp maybe_verify_undo_action(dsl_state, bulk_update) do
    error_path = [:bulk_update, bulk_update.name, :undo_action]

    with {:ok, action} <-
           get_action(dsl_state, bulk_update.resource, bulk_update.undo_action, error_path),
         :ok <- verify_action_type(dsl_state, bulk_update.resource, action, :action, error_path) do
      verify_action_takes_bulk_result(dsl_state, bulk_update.resource, action, error_path)
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
         "The undo action for a bulk update step should take a single `bulk_result` argument."
     )}
  end
end
