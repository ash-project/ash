defmodule Ash.Actions.Action do
  @moduledoc false

  require Ash.Tracer

  def run(_api, %{valid?: false, errors: errors}, _opts) do
    {:error, Ash.Error.to_error_class(errors)}
  end

  def run(api, input, opts) do
    {input, opts} = Ash.Actions.Helpers.add_process_context(api, input, opts)

    context =
      Map.merge(input.context, %{
        actor: opts[:actor],
        tenant: opts[:tenant],
        authorize?: opts[:authorize?],
        api: opts[:api]
      })

    {module, run_opts} = input.action.run

    Ash.Tracer.span :action,
                    Ash.Api.Info.span_name(
                      api,
                      input.resource,
                      input.action.name
                    ),
                    opts[:tracer] do
      metadata = %{
        api: api,
        resource: input.resource,
        resource_short_name: Ash.Resource.Info.short_name(input.resource),
        actor: opts[:actor],
        tenant: opts[:tenant],
        action: input.action.name,
        authorize?: opts[:authorize?]
      }

      Ash.Tracer.set_metadata(opts[:tracer], :action, metadata)

      Ash.Tracer.telemetry_span [:ash, Ash.Api.Info.short_name(api), :create],
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
            resources
            |> Enum.reject(&Ash.DataLayer.in_transaction?/1)
            |> Ash.DataLayer.transaction(
              fn ->
                case authorize(api, opts[:actor], input) do
                  :ok ->
                    case module.run(input, run_opts, context) do
                      {:ok, result} ->
                        {:ok, result, []}

                      other ->
                        other
                    end

                  {:error, error} ->
                    {:error, error}
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
          case authorize(api, opts[:actor], input) do
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
                  raise """
                  Invalid return from generic action #{input.resource}.#{input.action.name}.

                  Expected {:ok, result} or {:error, error}, got:

                  #{inspect(other)}
                  """
              end

            {:error, error} ->
              {:error, error}
          end
        end
      end
    end
  end

  defp authorize(api, actor, input) do
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
          api: api,
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
