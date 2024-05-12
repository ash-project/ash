defmodule Ash.Actions.Create do
  @moduledoc false

  alias Ash.Actions.Helpers

  require Ash.Tracer
  require Logger

  @spec run(Ash.Domain.t(), Ash.Changeset.t(), Ash.Resource.Actions.action(), Keyword.t()) ::
          {:ok, Ash.Resource.record(), list(Ash.Notifier.Notification.t())}
          | {:ok, Ash.Resource.record()}
          | {:error, term}
  def run(domain, changeset, action, opts) do
    if changeset.atomics != [] &&
         !Ash.DataLayer.data_layer_can?(changeset.resource, {:atomic, :upsert}) do
      {:error,
       Ash.Error.Invalid.AtomicsNotSupported.exception(
         resource: changeset.resource,
         action_type: :create
       )}
    else
      {changeset, opts} = Ash.Actions.Helpers.set_context_and_get_opts(domain, changeset, opts)
      changeset = Helpers.apply_opts_load(changeset, opts)

      Ash.Tracer.span :action,
                      Ash.Domain.Info.span_name(
                        domain,
                        changeset.resource,
                        action.name
                      ),
                      opts[:tracer] do
        metadata = %{
          domain: domain,
          resource: changeset.resource,
          resource_short_name: Ash.Resource.Info.short_name(changeset.resource),
          actor: opts[:actor],
          tenant: opts[:tenant],
          action: action.name,
          authorize?: opts[:authorize?]
        }

        Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

        Ash.Tracer.telemetry_span [:ash, Ash.Domain.Info.short_name(domain), :create],
                                  metadata do
          case do_run(domain, changeset, action, opts) do
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
    end
  rescue
    e ->
      reraise Ash.Error.to_error_class(e, changeset: changeset, stacktrace: __STACKTRACE__),
              __STACKTRACE__
  end

  defp do_run(domain, changeset, action, opts) do
    opts =
      opts
      |> Keyword.put(
        :upsert?,
        action.upsert? || opts[:upsert?] || get_in(changeset.context, [:private, :upsert?]) ||
          false
      )

    upsert_identity =
      if opts[:upsert?] do
        action.upsert_identity || opts[:upsert_identity] ||
          get_in(changeset.context, [:private, :upsert_identity])
      else
        opts[:upsert_identity] || get_in(changeset.context, [:private, :upsert_identity])
      end

    opts =
      Keyword.put(opts, :upsert_identity, upsert_identity)

    changeset =
      Ash.Changeset.set_context(changeset, %{
        private: %{upsert?: true, upsert_identity: upsert_identity}
      })

    with %{valid?: true} = changeset <- changeset(changeset, domain, action, opts),
         %{valid?: true} = changeset <- check_upsert_support(changeset, opts),
         %{valid?: true} = changeset <- authorize(changeset, opts),
         {:ok, result, instructions} <- commit(changeset, domain, opts) do
      add_notifications(
        changeset.resource,
        result,
        changeset.action,
        instructions,
        opts[:return_notifications?]
      )
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
        {:error, Ash.Error.to_error_class(errors, changeset: changeset)}
    end
  end

  defp authorize(changeset, opts) do
    if opts[:authorize?] do
      case Ash.can(changeset, opts[:actor],
             alter_source?: true,
             pre_flight?: false,
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

  defp add_tenant({:ok, nil}, _), do: {:ok, nil}

  defp add_tenant({:ok, data}, changeset) do
    if changeset.tenant do
      {:ok, %{data | __metadata__: Map.put(data.__metadata__, :tenant, changeset.tenant)}}
    else
      {:ok, data}
    end
  end

  defp add_tenant(other, _), do: other

  defp add_notifications(
         resource,
         result,
         action,
         instructions,
         return_notifications?
       ) do
    if return_notifications? do
      {:ok, result, Map.get(instructions, :notifications, [])}
    else
      Helpers.warn_missed!(resource, action, instructions)

      {:ok, result}
    end
  end

  defp changeset(changeset, domain, action, opts) do
    changeset = %{changeset | domain: domain}

    if changeset.__validated_for_action__ == action.name do
      changeset
    else
      Ash.Changeset.for_create(changeset, action.name, %{}, opts)
    end
    |> Ash.Changeset.set_defaults(:create, true)
    |> Ash.Changeset.timeout(opts[:timeout] || changeset.timeout)
  end

  defp commit(changeset, domain, opts) do
    upsert_keys =
      case opts[:upsert_identity] do
        nil ->
          Ash.Resource.Info.primary_key(changeset.resource)

        identity ->
          keys =
            changeset.resource
            |> Ash.Resource.Info.identities()
            |> Enum.find(&(&1.name == identity))
            |> Kernel.||(
              raise Ash.Error.Invalid.NoIdentityFound,
                resource: changeset.resource,
                identity: identity
            )
            |> Map.get(:keys)

          if changeset.tenant &&
               Ash.Resource.Info.multitenancy_strategy(changeset.resource) == :attribute do
            [Ash.Resource.Info.multitenancy_attribute(changeset.resource) | keys]
          else
            keys
          end
      end

    changeset = set_tenant(changeset)

    can_atomic_create? =
      Ash.DataLayer.data_layer_can?(changeset.resource, {:atomic, :upsert})

    result =
      changeset
      |> Ash.Changeset.with_hooks(
        fn
          %{atomics: atomics} when atomics != [] and not can_atomic_create? ->
            {:error,
             Ash.Error.Invalid.AtomicsNotSupported.exception(
               resource: changeset.resource,
               action_type: :create
             )}

          changeset ->
            changeset = Ash.Changeset.hydrate_atomic_refs(changeset, opts[:actor])

            case Ash.Actions.ManagedRelationships.setup_managed_belongs_to_relationships(
                   changeset,
                   opts[:actor],
                   authorize?: opts[:authorize?],
                   actor: opts[:actor]
                 ) do
              {:error, error} ->
                {:error, error}

              {changeset, manage_instructions} ->
                changeset =
                  if changeset.context[:private][:action_result] do
                    changeset
                  else
                    Ash.Changeset.require_values(
                      changeset,
                      :create
                    )
                    |> Ash.Changeset.require_values(
                      :update,
                      false,
                      changeset.action.require_attributes
                    )
                  end

                if changeset.valid? do
                  if changeset.action.manual do
                    {mod, action_opts} = changeset.action.manual

                    if result = changeset.context[:private][:action_result] do
                      result
                    else
                      mod.create(changeset, action_opts, %Ash.Resource.ManualCreate.Context{
                        select: opts[:select],
                        actor: opts[:actor],
                        tenant: changeset.tenant,
                        tracer: opts[:tracer],
                        authorize?: opts[:authorize?],
                        domain: changeset.domain,
                        upsert?: opts[:upsert?],
                        upsert_keys: upsert_keys,
                        upsert_fields: changeset.context[:private][:upsert_fields]
                      })
                      |> validate_manual_action_return_result!(
                        changeset.resource,
                        changeset.action
                      )
                    end
                    |> add_tenant(changeset)
                    |> manage_relationships(domain, changeset,
                      actor: opts[:actor],
                      authorize?: opts[:authorize?],
                      upsert?: opts[:upsert?]
                    )
                  else
                    belongs_to_attrs =
                      changeset.resource
                      |> Ash.Resource.Info.relationships()
                      |> Enum.filter(&(&1.type == :belongs_to))
                      |> Enum.map(& &1.source_attribute)

                    final_check =
                      changeset.resource
                      |> Ash.Resource.Info.attributes()
                      |> Enum.reject(
                        &(&1.allow_nil? || &1.generated? || &1.name in belongs_to_attrs)
                      )

                    changeset =
                      if changeset.context[:private][:action_result] do
                        changeset
                      else
                        changeset =
                          changeset
                          |> Ash.Changeset.require_values(
                            :create,
                            true,
                            final_check
                          )

                        {changeset, _} =
                          Ash.Actions.ManagedRelationships.validate_required_belongs_to(
                            {changeset, []}
                          )

                        changeset
                      end

                    if changeset.valid? do
                      cond do
                        result = changeset.context[:private][:action_result] ->
                          result
                          |> add_tenant(changeset)
                          |> manage_relationships(domain, changeset,
                            actor: opts[:actor],
                            authorize?: opts[:authorize?],
                            upsert?: opts[:upsert?]
                          )

                        opts[:upsert?] ->
                          changeset.resource
                          |> Ash.DataLayer.upsert(changeset, upsert_keys)
                          |> Helpers.rollback_if_in_transaction(
                            changeset.resource,
                            changeset
                          )
                          |> add_tenant(changeset)
                          |> manage_relationships(domain, changeset,
                            actor: opts[:actor],
                            authorize?: opts[:authorize?],
                            upsert?: opts[:upsert?]
                          )

                        true ->
                          case Ash.Changeset.handle_allow_nil_atomics(changeset, opts[:actor]) do
                            %Ash.Changeset{valid?: true} = changeset ->
                              changeset.resource
                              |> Ash.DataLayer.create(changeset)
                              |> Helpers.rollback_if_in_transaction(
                                changeset.resource,
                                changeset
                              )
                              |> add_tenant(changeset)
                              |> manage_relationships(domain, changeset,
                                actor: opts[:actor],
                                authorize?: opts[:authorize?],
                                upsert?: opts[:upsert?]
                              )

                            %Ash.Changeset{} = changeset ->
                              {:error, changeset}
                          end
                      end
                      |> case do
                        {:ok, result, instructions} ->
                          {:ok, result,
                           instructions
                           |> Map.update!(
                             :notifications,
                             &(&1 ++ manage_instructions.notifications)
                           )}

                        {:error, error} ->
                          {:error, Ash.Changeset.add_error(changeset, error)}
                      end
                    else
                      {:error, changeset}
                    end
                  end
                else
                  {:error, changeset}
                end
            end
        end,
        transaction?: Keyword.get(opts, :transaction?, true) && changeset.action.transaction?,
        rollback_on_error?: opts[:rollback_on_error?],
        tracer: opts[:tracer],
        return_notifications?: opts[:return_notifications?],
        transaction_metadata: %{
          type: :create,
          metadata: %{
            resource: changeset.resource,
            action: changeset.action.name,
            actor: opts[:actor]
          }
        }
      )

    case result do
      {:ok, created, changeset, instructions} ->
        {:ok, created, instructions}
        |> Helpers.load(changeset, domain,
          actor: opts[:actor],
          reuse_values?: true,
          authorize?: opts[:authorize?],
          tracer: opts[:tracer]
        )
        |> Helpers.notify(changeset, opts)
        |> Helpers.select(changeset)
        |> Helpers.restrict_field_access(changeset)

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
    raise Ash.Error.Framework.InvalidReturnType,
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

  defp manage_relationships({:ok, nil}, _, _, _) do
    {:ok, nil, %{notifications: []}}
  end

  defp manage_relationships(
         {:ok, created, %{notifications: notifications}},
         domain,
         changeset,
         engine_opts
       ) do
    case manage_relationships({:ok, created}, domain, changeset, engine_opts) do
      {:ok, created, info} ->
        {:ok, created, Map.update(info, :notifications, notifications, &(&1 ++ notifications))}

      other ->
        other
    end
  end

  defp manage_relationships({:ok, created}, domain, changeset, engine_opts) do
    with {:ok, loaded} <-
           Ash.Actions.ManagedRelationships.load(domain, created, changeset, engine_opts),
         {:ok, with_relationships, new_notifications} <-
           Ash.Actions.ManagedRelationships.manage_relationships(
             loaded,
             changeset,
             engine_opts[:actor],
             engine_opts
           ) do
      {:ok, with_relationships, %{notifications: new_notifications, new_changeset: changeset}}
    end
  end

  defp manage_relationships(other, _, _, _), do: other

  defp set_tenant(changeset) do
    if changeset.tenant &&
         Ash.Resource.Info.multitenancy_strategy(changeset.resource) == :attribute do
      attribute = Ash.Resource.Info.multitenancy_attribute(changeset.resource)
      {m, f, a} = Ash.Resource.Info.multitenancy_parse_attribute(changeset.resource)
      attribute_value = apply(m, f, [changeset.to_tenant | a])

      Ash.Changeset.force_change_attribute(changeset, attribute, attribute_value)
    else
      changeset
    end
  end

  defp check_upsert_support(changeset, opts) do
    if opts[:upsert?] && !Ash.DataLayer.data_layer_can?(changeset.resource, :upsert) do
      Ash.Changeset.add_error(
        changeset,
        "Upsert is not supported by the data layer for this resource"
      )
    else
      changeset
    end
  end
end
