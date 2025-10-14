# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Actions.Action do
  @moduledoc false

  require Ash.Tracer

  def run(_domain, %{valid?: false, errors: errors}, _opts) do
    {:error, Ash.Error.to_error_class(errors)}
  end

  def run(domain, input, opts) do
    {input, opts} = Ash.Actions.Helpers.set_context_and_get_opts(domain, input, opts)

    if input.valid? do
      run_with_lifecycle(domain, input, opts)
    else
      {:error, Ash.Error.to_error_class(input.errors)}
    end
  end

  defp run_with_lifecycle(domain, input, opts) do
    context =
      %Ash.Resource.Actions.Implementation.Context{
        actor: opts[:actor],
        tenant: opts[:tenant],
        tracer: opts[:tracer],
        source_context: input.context,
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

      Ash.Tracer.telemetry_span [:ash, Ash.Domain.Info.short_name(domain), :action],
                                metadata do
        # Run around_transaction hooks if any exist, or proceed directly
        Ash.ActionInput.run_around_transaction_hooks(input, fn input ->
          if input.action.transaction? do
            run_with_transaction(domain, input, module, run_opts, context, opts)
          else
            run_without_transaction(domain, input, module, run_opts, context, opts)
          end
        end)
      end
    end
  end

  defp run_with_transaction(domain, input, module, run_opts, context, opts) do
    # Run before_transaction hooks first
    case Ash.ActionInput.run_before_transaction_hooks(input) do
      {:ok, input} ->
        notify? = !Process.put(:ash_started_transaction?, true)

        try do
          resources =
            input.action.touches_resources
            |> Enum.reject(&Ash.DataLayer.in_transaction?/1)
            |> Enum.concat([input.resource])
            |> Enum.uniq()

          resources
          |> Ash.DataLayer.transaction(
            fn ->
              case authorize(domain, opts[:actor], input) do
                :ok ->
                  case run_with_hooks(module, input, run_opts, context, true) do
                    {:ok, result, notifications} ->
                      {:ok, result, notifications}

                    {:error, error} ->
                      Ash.DataLayer.rollback(resources, error)
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
            },
            rollback_on_error?: false
          )
          |> case do
            {:ok, {:ok, result, notifications}} ->
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

              final_result =
                if input.action.returns do
                  {:ok, result}
                else
                  :ok
                end

              # Run after_transaction hooks
              Ash.ActionInput.run_after_transaction_hooks(final_result, input)

            {:error, error} ->
              error_result = {:error, Ash.Error.to_ash_error(error)}
              # Run after_transaction hooks even on error
              Ash.ActionInput.run_after_transaction_hooks(error_result, input)
          end
        after
          if notify? do
            Process.delete(:ash_started_transaction?)
          end
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp run_without_transaction(domain, input, module, run_opts, context, opts) do
    # Run before_transaction hooks even for non-transactional actions
    case Ash.ActionInput.run_before_transaction_hooks(input) do
      {:ok, input} ->
        result =
          case authorize(domain, opts[:actor], input) do
            :ok ->
              case run_with_hooks(module, input, run_opts, context, false) do
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
              end

            {:error, error} ->
              {:error, error}
          end

        # Run after_transaction hooks
        Ash.ActionInput.run_after_transaction_hooks(result, input)

      {:error, error} ->
        {:error, error}
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

      arguments =
        Enum.reduce(module.reactor().inputs, input.arguments, fn input, arguments ->
          Map.put_new(arguments, input, nil)
        end)

      Reactor.run(
        module,
        arguments,
        context,
        run_opts
      )
    else
      Ash.Resource.Actions.Implementation.run(module, input, run_opts, context)
    end
  end

  defp raise_invalid_generic_action_return!(input, other) do
    ok_or_ok_tuple = if input.action.returns, do: "{:ok, result}", else: ":ok"

    raise Ash.Error.Framework.InvalidReturnType,
      message: """
      Invalid return from generic action #{input.resource}.#{input.action.name}.

      Expected #{ok_or_ok_tuple} or {:error, error}, got:

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

  defp run_with_hooks(module, input, run_opts, context, in_transaction?) do
    # Run before_action hooks
    case Ash.ActionInput.run_before_actions(input) do
      {:error, error} ->
        if in_transaction? do
          Ash.DataLayer.rollback([input.resource], error)
        else
          {:error, error}
        end

      {input, %{notifications: before_action_notifications}} ->
        # Run the actual action
        case call_run_function(module, input, run_opts, context, in_transaction?) do
          :ok when is_nil(input.action.returns) ->
            # Run after_action hooks
            case Ash.ActionInput.run_after_actions(nil, input, before_action_notifications) do
              {:ok, result, _input, %{notifications: all_notifications}} ->
                {:ok, result, all_notifications}

              {:error, error} ->
                if in_transaction? do
                  Ash.DataLayer.rollback([input.resource], error)
                else
                  {:error, error}
                end
            end

          {:ok, result} ->
            if input.action.returns do
              # Run after_action hooks
              case Ash.ActionInput.run_after_actions(result, input, before_action_notifications) do
                {:ok, result, _input, %{notifications: all_notifications}} ->
                  {:ok, result, all_notifications}

                {:error, error} ->
                  if in_transaction? do
                    Ash.DataLayer.rollback([input.resource], error)
                  else
                    {:error, error}
                  end
              end
            else
              raise_invalid_generic_action_return!(input, result)
            end

          {:ok, result, notifications} ->
            # Run after_action hooks
            case Ash.ActionInput.run_after_actions(
                   result,
                   input,
                   before_action_notifications ++ notifications
                 ) do
              {:ok, result, _input, %{notifications: all_notifications}} ->
                {:ok, result, all_notifications}

              {:error, error} ->
                if in_transaction? do
                  Ash.DataLayer.rollback([input.resource], error)
                else
                  {:error, error}
                end
            end

          {:error, error} ->
            if in_transaction? do
              Ash.DataLayer.rollback([input.resource], error)
            else
              {:error, error}
            end

          other ->
            raise_invalid_generic_action_return!(input, other)
        end
    end
  end
end
