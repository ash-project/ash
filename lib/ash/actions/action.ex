defmodule Ash.Actions.Action do
  @moduledoc false

  require Ash.Tracer

  def run(_domain, %{valid?: false, errors: errors}, _opts) do
    {:error, Ash.Error.to_error_class(errors)}
  end

  def run(domain, input, opts) do
    {input, opts} = Ash.Actions.Helpers.set_context_and_get_opts(domain, input, opts)

    context =
      %Ash.Resource.Actions.Implementation.Context{
        actor: opts[:actor],
        tenant: opts[:tenant],
        authorize?: opts[:authorize?],
        domain: opts[:domain]
      }

    {module, run_opts} = input.action.run

    Ash.Tracer.span :action,
                    fn ->
                      Ash.Domain.Info.span_name(
                        domain,
                        input.resource,
                        input.action.name
                      )
                    end,
                    opts[:tracer] do
      metadata = fn ->
        %{
          domain: domain,
          resource: input.resource,
          resource_short_name: Ash.Resource.Info.short_name(input.resource),
          actor: opts[:actor],
          tenant: opts[:tenant],
          action: input.action.name,
          authorize?: opts[:authorize?]
        }
      end

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
                    case call_run_function(module, input, run_opts, context, true) do
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
              case call_run_function(module, input, run_opts, context, false) do
                :ok when is_nil(input.action.returns) ->
                  :ok

                {:ok, result} ->
                  if input.action.returns do
                    {:ok, result}
                  else
                    :ok
                  end

                {:ok, result, notifications} ->
                  remaining = Ash.Notifier.notify(notifications)

                  Ash.Actions.Helpers.warn_missed!(input.resource, input.action, %{
                    resource_notifications: remaining
                  })

                  if input.action.returns do
                    {:ok, result}
                  else
                    :ok
                  end

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

  defp is_reactor?(module) when is_atom(module) do
    module.spark_is() == Reactor
  rescue
    UndefinedFunctionError -> false
  end

  defp call_run_function(module, input, run_opts, context, in_transaction?) do
    if is_reactor?(module) do
      run_opts =
        if in_transaction?,
          do: Keyword.put(run_opts, :async?, false),
          else: run_opts

      context =
        context
        |> Ash.Context.to_opts()
        |> Map.new()

      Reactor.run(
        module,
        input.arguments,
        context,
        run_opts
      )
    else
      module.run(input, run_opts, context)
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
            input.domain
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
