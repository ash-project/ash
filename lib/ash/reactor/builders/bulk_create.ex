defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.BulkCreate do
  @moduledoc false

  alias Ash.Reactor.BulkCreateStep
  alias Ash.Resource.Info
  alias Reactor.{Argument, Builder}
  alias Spark.{Dsl.Transformer, Error.DslError}
  import Ash.Reactor.BuilderUtils
  import Reactor.Template, only: :macros

  @doc false
  @impl true
  def build(bulk_create, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor) do
      initial = %Argument{name: :initial, source: bulk_create.initial}

      notification_metadata =
        case bulk_create.notification_metadata do
          template when is_template(template) ->
            %Argument{name: :notification_metadata, source: template}

          map when is_map(map) ->
            Argument.from_value(:notification_metadata, map)
        end

      arguments =
        [initial, notification_metadata]
        |> maybe_append(bulk_create.actor)
        |> maybe_append(bulk_create.tenant)
        |> maybe_append(bulk_create.load)
        |> maybe_append(bulk_create.context)
        |> Enum.concat(bulk_create.wait_for)

      action_options =
        bulk_create
        |> Map.take([
          :action,
          :assume_casted?,
          :authorize_changeset_with,
          :authorize_query_with,
          :authorize?,
          :batch_size,
          :domain,
          :max_concurrency,
          :notify?,
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
          :success_state,
          :timeout,
          :transaction,
          :upsert_fields,
          :upsert_fields,
          :upsert_identity,
          :upsert_identity,
          :upsert?,
          :undo_action,
          :undo
        ])
        |> Map.put(:return_notifications?, bulk_create.notify?)
        |> Enum.reject(&is_nil(elem(&1, 1)))

      step_options =
        bulk_create
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        bulk_create.name,
        {BulkCreateStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def verify(bulk_create, dsl_state) do
    action_error_path = [:bulk_create, bulk_create.name, :action]

    with {:ok, action} <-
           get_action(dsl_state, bulk_create.resource, bulk_create.action, action_error_path),
         :ok <-
           verify_action_type(dsl_state, bulk_create.resource, action, :create, action_error_path),
         :ok <- verify_undo(dsl_state, bulk_create),
         :ok <- maybe_verify_undo_action(dsl_state, bulk_create),
         :ok <- maybe_verify_upsert_fields(dsl_state, bulk_create, action, action_error_path),
         :ok <- verify_select(dsl_state, bulk_create),
         :ok <- verify_rollback_on_error(dsl_state, bulk_create),
         :ok <- verify_sorted(dsl_state, bulk_create) do
      verify_notify(dsl_state, bulk_create)
    end
  end

  defguardp is_falsy(value) when value in [nil, false]

  defp verify_notify(dsl_state, bulk_create)
       when bulk_create.notify? == true and bulk_create.return_stream? == true,
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_create, bulk_create.name, :notify?],
            message: """
            Setting `notify?` has no effect when `return_stream?` is `true`.

            You must manually consume the resulting stream of records and notifications in a subsequent step.
            """
          )}

  defp verify_notify(_dsl_state, _bulk_create), do: :ok

  defp verify_sorted(dsl_state, bulk_create)
       when bulk_create.sorted? == true and is_falsy(bulk_create.return_records?),
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_create, bulk_create.name, :sorted?],
            message: "Setting `sorted?` has no effect with `return_records?` is not `true`."
          )}

  defp verify_sorted(_dsl_state, _bulk_create), do: :ok

  defp verify_rollback_on_error(dsl_state, bulk_create)
       when bulk_create.rollback_on_error? == true and is_falsy(bulk_create.transaction),
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_create, bulk_create.name, :rollback_on_error?],
            message: "Setting `rollback_on_error?` has no effect when `transaction` is `false`."
          )}

  defp verify_rollback_on_error(_dsl_state, _bulk_create), do: :ok

  defp verify_select(_dsl_state, bulk_create) when bulk_create.select == [], do: :ok
  defp verify_select(_dsl_state, bulk_create) when bulk_create.return_records? == true, do: :ok

  defp verify_select(dsl_state, bulk_create),
    do:
      {:error,
       DslError.exception(
         module: Transformer.get_persisted(dsl_state, :module),
         path: [:bulk_create, bulk_create.name, :select],
         message: "Setting `select` has no effect when `return_records?` is not `true`."
       )}

  defp maybe_verify_upsert_fields(dsl_state, bulk_create, action, error_path)
       when bulk_create.upsert? == true and bulk_create.upsert_fields == [] and
              action.upsert_fields == [],
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: error_path,
            message:
              "Expected `upsert_fields` to be set on either the bulk create step or the underlying action."
          )}

  defp maybe_verify_upsert_fields(_dsl_state, _bulk_create, _action, _error_path), do: :ok

  defp verify_undo(dsl_state, bulk_create)
       when bulk_create.undo != :never and bulk_create.return_stream? == true,
       do:
         {:error,
          DslError.exception(
            module: Transformer.get_persisted(dsl_state, :module),
            path: [:bulk_create, bulk_create.name, :undo],
            message:
              "Cannot set undo to anything other than `:never` when `return_stream?` is `true`."
          )}

  defp verify_undo(_dsl_state, _bulk_create), do: :ok

  defp maybe_verify_undo_action(_dsl_state, bulk_create) when bulk_create.undo == :never, do: :ok

  defp maybe_verify_undo_action(dsl_state, bulk_create) do
    error_path = [:bulk_create, bulk_create.name, :undo_action]

    with {:ok, action} <-
           get_action(dsl_state, bulk_create.resource, bulk_create.undo_action, error_path),
         :ok <- verify_action_type(dsl_state, bulk_create.resource, action, :action, error_path) do
      verify_action_takes_bulk_result(dsl_state, bulk_create.resource, action, error_path)
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
         "The undo action for a bulk create step should take a single `bulk_result` argument."
     )}
  end
end
