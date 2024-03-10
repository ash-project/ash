defmodule Ash.Actions.Action do
  @moduledoc false

  require Ash.Tracer

  def run(_domain, %{valid?: false, errors: errors}, _opts) do
    {:error, Ash.Error.to_error_class(errors)}
  end

  def run(domain, input, opts) do
    {input, opts} = Ash.Actions.Helpers.add_process_context(domain, input, opts)

    context =
      %Ash.Resource.Actions.Implementation.Context{
        actor: opts[:actor],
        tenant: opts[:tenant],
        authorize?: opts[:authorize?],
        domain: opts[:domain]
      }

    {module, run_opts} = input.action.run

    Ash.Tracer.span :action,
                    Ash.Domain.Info.span_name(
                      domain,
                      input.resource,
                      input.action.name
                    ),
                    opts[:tracer] do
      metadata = %{
        domain: domain,
        resource: input.resource,
        resource_short_name: Ash.Resource.Info.short_name(input.resource),
        actor: opts[:actor],
        tenant: opts[:tenant],
        action: input.action.name,
        authorize?: opts[:authorize?]
      }

      Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

      Ash.Tracer.telemetry_span [:ash, Ash.Domain.Info.short_name(domain), :create],
                                metadata do
        if input.action.transaction? do
          resources =
            input.resource
            |> List.wrap()
            |> Enum.concat(input.action.touches_resources)
            |> Enum.uniq()

          notify? =
            if Process.get(:ash_started_transaction?) do
              false
            else
              Process.put(:ash_started_transaction?, true)
              true
            end

          try do
            resources = Enum.reject(resources, &Ash.DataLayer.in_transaction?/1)

            resources
            |> Ash.DataLayer.transaction(
              fn ->
                case authorize(domain, opts[:actor], input) do
                  :ok ->
                    case module.run(input, run_opts, context) do
                      {:ok, result} ->
                        {:ok, result, []}

                      {:error, error} ->
                        Ash.DataLayer.rollback(resources, error)

                      other ->
                        raise_invalid_manual_action_return!(input, other)
                    end

                  {:error, error} ->
                    Ash.DataLayer.rollback(resources, error)
                end
              end,
              nil,
              %{
                type: :generic,
                metadata: %{
                  resource: input.resource,
                  action: input.action.name,
                  input: input,
                  actor: opts[:actor]
                },
                data_layer_context: input.context[:data_layer] || %{}
              }
            )
            |> case do
              {:ok, {:ok, value, notifications}} ->
                notifications =
                  if notify? && !opts[:return_notifications?] do
                    Enum.concat(
                      notifications || [],
                      Process.delete(:ash_notifications) || []
                    )
                  else
                    notifications || []
                  end

                remaining = Ash.Notifier.notify(notifications)

                Ash.Actions.Helpers.warn_missed!(input.resource, input.action, %{
                  resource_notifications: remaining
                })

                {:ok, value}

              {:error, error} ->
                {:error, Ash.Error.to_ash_error(error)}
            end
          after
            if notify? do
              Process.delete(:ash_started_transaction?)
            end
          end
        else
          case authorize(domain, opts[:actor], input) do
            :ok ->
              case module.run(input, run_opts, context) do
                {:ok, result} ->
                  {:ok, result}

                {:ok, result, notifications} ->
                  remaining = Ash.Notifier.notify(notifications)

                  Ash.Actions.Helpers.warn_missed!(input.resource, input.action, %{
                    resource_notifications: remaining
                  })

                  {:ok, result}

                {:error, error} ->
                  {:error, error}

                other ->
                  raise_invalid_manual_action_return!(input, other)
              end

            {:error, error} ->
              {:error, error}
          end
        end
      end
    end
  end

  defp raise_invalid_manual_action_return!(input, other) do
    raise """
    Invalid return from generic action #{input.resource}.#{input.action.name}.

    Expected {:ok, result} or {:error, error}, got:

    #{inspect(other)}
    """
  end

  defp authorize(_domain, _actor, %{context: %{private: %{authorize?: false}}}) do
    :ok
  end

  defp authorize(domain, actor, input) do
    input.resource
    |> Ash.Resource.Info.authorizers()
    |> Enum.reduce_while(
      :ok,
      fn authorizer, :ok ->
        authorizer_state =
          authorizer.initial_state(
            actor,
            input.resource,
            input.action,
            false
          )

        context = %{
          domain: domain,
          action_input: input,
          query: nil,
          changeset: nil
        }

        case authorizer.strict_check(authorizer_state, context) do
          {:error, %{class: :forbidden} = e} when is_exception(e) ->
            {:halt, {:error, e}}

          {:error, error} ->
            {:halt, {:error, error}}

          {:authorized, _} ->
            {:cont, :ok}

          {:filter, _authorizer, filter} ->
            raise """
            Cannot use filter checks with generic actions

            Received #{inspect(filter)} when authorizing #{inspect(input.resource)}.#{input.action.name}
            """

          {:filter, filter} ->
            raise """
            Cannot use filter checks with generic actions

            Received #{inspect(filter)} when authorizing #{inspect(input.resource)}.#{input.action.name}
            """

          {:continue, _state} ->
            raise """
            Cannot use runtime checks with generic actions

            Must use only simple checks or other checks that can be resolved without returning results #{inspect(input.resource)}.#{input.action.name}
            """

          {:filter_and_continue, filter, _} ->
            raise """
            Cannot use filter checks with generic actions

            Received #{inspect(filter)} when authorizing #{inspect(input.resource)}.#{input.action.name}
            """

          :forbidden ->
            {:halt, {:error, Ash.Authorizer.exception(authorizer, :forbidden, authorizer_state)}}
        end
      end
    )
  end
end
