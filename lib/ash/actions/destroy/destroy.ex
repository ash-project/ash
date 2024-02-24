defmodule Ash.Actions.Destroy do
  @moduledoc false

  alias Ash.Actions.Helpers

  require Ash.Tracer

  @spec run(Ash.Domain.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, list(Ash.Notifier.Notification.t())}
          | :ok
          | {:error, Ash.Changeset.t()}
          | {:error, term}
  def run(domain, changeset, %{soft?: true} = action, opts) do
    changeset =
      if changeset.__validated_for_action__ == action.name do
        %{changeset | action_type: :destroy}
      else
        Ash.Changeset.for_destroy(%{changeset | action_type: :destroy}, action.name, %{},
          actor: opts[:actor]
        )
      end

    Ash.Actions.Update.run(domain, changeset, action, opts)
  end

  def run(domain, changeset, action, opts) do
    {changeset, opts} = Ash.Actions.Helpers.add_process_context(domain, changeset, opts)

    Ash.Tracer.span :action,
                    Ash.Api.Info.span_name(
                      api,
                      changeset.resource,
                      action.name
                    ),
                    opts[:tracer] do
      metadata = %{
        api: api,
        resource: changeset.resource,
        resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
        actor: opts[:actor],
        tenant: opts[:tenant],
        action: action.name,
        authorize?: opts[:authorize?]
      }

      Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

      Ash.Tracer.telemetry_span [:ash, Ash.Api.Info.short_name(api), :destroy], metadata do
        case do_run(api, changeset, action, opts) do
          {:error, error} ->
            if opts[:tracer] do
              stacktrace =
                case error do
                  %{stacktrace: %{stacktrace: stacktrace}} ->
                    stacktrace || []

                  _ ->
                    {:current_stacktrace, stacktrace} =
                      Process.info(self(), :current_stacktrace)

                    stacktrace
                end

              Ash.Tracer.set_handled_error(opts[:tracer], Ash.Error.to_error_class(error),
                stacktrace: stacktrace
              )
            end

            {:error, error}

          other ->
            other
        end
      end
    end
  rescue
    e ->
      reraise Ash.Error.to_error_class(e, changeset: changeset, stacktrace: __STACKTRACE__),
              __STACKTRACE__
  end

  def do_run(domain, changeset, action, opts) do
    {changeset, opts} = Ash.Actions.Helpers.add_process_context(domain, changeset, opts)

    return_destroyed? = opts[:return_destroyed?]
    changeset = %{changeset | domain: domain}

    changeset =
      if opts[:tenant] do
        Ash.Changeset.set_tenant(changeset, opts[:tenant])
      else
        changeset
      end

    with %{valid?: true} = changeset <- Ash.Changeset.validate_multitenancy(changeset),
         %{valid?: true} = changeset <- changeset(changeset, domain, action, opts),
         %{valid?: true} = changeset <- authorize(changeset, domain, opts),
         {:commit, {:ok, result, instructions}} <- {:commit, commit(changeset, domain, opts)} do
      changeset.resource
      |> add_notifications(
        changeset.action,
        instructions,
        opts[:return_notifications?]
      )
      |> add_destroyed(return_destroyed?, result)
    end
    |> case do
      {:ok, result} ->
        {:ok, result}

      {:ok, result, notifications} ->
        {:ok, result, notifications}

      :ok ->
        :ok

      %Ash.Changeset{errors: errors} = changeset ->
        errors = Helpers.process_errors(changeset, errors)

        Ash.Changeset.run_after_transactions(
          {:error, Ash.Error.to_error_class(errors, changeset: changeset)},
          changeset
        )

      {:error, error} ->
        errors = Helpers.process_errors(changeset, List.wrap(error))

        Ash.Changeset.run_after_transactions(
          {:error, Ash.Error.to_error_class(errors, changeset: changeset)},
          changeset
        )

      {:commit, {:error, error}} ->
        errors = Helpers.process_errors(changeset, List.wrap(error))
        {:error, Ash.Error.to_error_class(errors, changeset: changeset)}
    end
  end

  defp authorize(changeset, domain, opts) do
    if opts[:authorize?] do
      case domain.can(changeset, opts[:actor],
             alter_source?: true,
             return_forbidden_error?: true,
             maybe_is: false
           ) do
        {:ok, true, changeset} ->
          changeset

        {:ok, false, error} ->
          Ash.Changeset.add_error(changeset, error)

        {:error, error} ->
          Ash.Changeset.add_error(changeset, error)
      end
    else
      changeset
    end
  end

  defp commit(changeset, domain, opts) do
    changeset
    |> Ash.Changeset.put_context(:private, %{actor: opts[:actor], authorize?: opts[:authorize?]})
    |> Ash.Changeset.with_hooks(
      fn
        %{valid?: false} = changeset ->
          {:error, changeset}

        changeset ->
          case Helpers.load({:ok, changeset.data, %{}}, changeset, domain,
                 actor: opts[:actor],
                 authorize?: opts[:authorize?],
                 tracer: opts[:tracer]
               ) do
            {:ok, new_data, _} ->
              changeset = %{changeset | data: new_data}

              if changeset.action.manual do
                {mod, action_opts} = changeset.action.manual

                if result = changeset.context[:private][:action_result] do
                  result
                else
                  mod.destroy(changeset, action_opts, %{
                    actor: opts[:actor],
                    tenant: changeset.tenant,
                    authorize?: opts[:authorize?],
                    domain: changeset.domain
                  })
                  |> validate_manual_action_return_result!(changeset.resource, changeset.action)
                end
              else
                if result = changeset.context[:private][:action_result] do
                  result
                else
                  changeset.resource
                  |> Ash.DataLayer.destroy(changeset)
                  |> Ash.Actions.Helpers.rollback_if_in_transaction(changeset.resource, changeset)
                  |> case do
                    :ok ->
                      {:ok, data} = Ash.Changeset.apply_attributes(changeset, force?: true)

                      {:ok,
                       Ash.Resource.set_meta(data, %Ecto.Schema.Metadata{
                         state: :deleted,
                         schema: changeset.resource
                       })}

                    {:error, error} ->
                      {:error, Ash.Changeset.add_error(changeset, error)}
                  end
                end
              end
              |> then(fn result ->
                case result do
                  {:ok, destroyed} ->
                    if opts[:return_destroyed?] do
                      {:ok, destroyed, %{notifications: []}}
                      |> Helpers.notify(changeset, opts)
                      |> Helpers.select(changeset)
                      |> Helpers.restrict_field_access(changeset)
                    else
                      {:ok, destroyed, %{notifications: []}}
                      |> Helpers.notify(changeset, opts)
                    end

                  {:error, %Ash.Changeset{} = changeset} ->
                    {:error, changeset}

                  other ->
                    other
                end
              end)

            other ->
              other
          end
      end,
      transaction?: Keyword.get(opts, :transaction?, true) && changeset.action.transaction?,
      rollback_on_error?: opts[:rollback_on_error?],
      notification_metadata: opts[:notification_metadata],
      return_notifications?: opts[:return_notifications?],
      transaction_metadata: %{
        type: :destroy,
        metadata: %{
          actor: opts[:actor],
          record: changeset.data,
          resource: changeset.resource,
          action: changeset.action.name
        }
      }
    )
    |> case do
      {:ok, result, changeset, instructions} ->
        instructions =
          Map.update(
            instructions,
            :set_keys,
            %{changeset: changeset, notification_data: result},
            &Map.merge(&1, %{changeset: changeset, notification_data: result})
          )

        {:ok, Helpers.select(result, changeset), instructions}

      {:error, error} ->
        {:error, error}
    end
  end

  defp validate_manual_action_return_result!({:ok, %resource{}} = result, resource, _) do
    result
  end

  defp validate_manual_action_return_result!(
         {:ok, %resource{}, notifications} = result,
         resource,
         _
       )
       when is_list(notifications) do
    result
  end

  defp validate_manual_action_return_result!({:error, _error} = result, _resource, _) do
    result
  end

  defp validate_manual_action_return_result!(other, resource, action) do
    raise Ash.Error.Framework.AssumptionFailed,
      message: """
      Manual action #{inspect(action.name)} on #{inspect(resource)} returned an invalid result.

      Expected one of the following:

      * {:ok, %Resource{}}
      * {:ok, %Resource{}, notifications}
      * {:error, error}

      Got:

      #{inspect(other)}
      """
  end

  defp add_notifications(resource, action, instructions, return_notifications?) do
    if return_notifications? do
      {:ok, Map.get(instructions, :notifications, [])}
    else
      Ash.Actions.Helpers.warn_missed!(resource, action, instructions)
      :ok
    end
  end

  defp add_destroyed(:ok, true, destroyed) do
    {:ok, destroyed}
  end

  defp add_destroyed({:ok, notifications}, true, destroyed) do
    {:ok, destroyed, notifications}
  end

  defp add_destroyed(result, _, _) do
    result
  end

  defp changeset(changeset, domain, action, opts) do
    changeset = %{changeset | domain: domain}

    if changeset.__validated_for_action__ == action.name do
      changeset
    else
      Ash.Changeset.for_destroy(changeset, action.name, %{}, opts)
    end
    |> Ash.Changeset.timeout(opts[:timeout] || changeset.timeout)
  end
end
