defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Destroy do
  @moduledoc false

  alias Ash.Reactor.DestroyStep
  alias Ash.Resource.Info
  alias Reactor.{Argument, Builder}
  alias Spark.{Dsl.Transformer, Error.DslError}
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(destroy, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, destroy) do
      arguments =
        arguments
        |> maybe_append(destroy.actor)
        |> maybe_append(destroy.tenant)
        |> Enum.concat(destroy.wait_for)
        |> Enum.concat([%Argument{name: :initial, source: destroy.initial}])

      action_options =
        destroy
        |> Map.take([
          :action,
          :authorize?,
          :domain,
          :resource,
          :return_destroyed?,
          :undo_action,
          :undo
        ])
        |> Enum.to_list()

      step_options =
        destroy
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        destroy.name,
        {DestroyStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def transform(_destroy, dsl_state), do: {:ok, dsl_state}

  @doc false
  @impl true
  def verify(destroy, _dsl_state) when destroy.undo == :never, do: :ok

  def verify(destroy, dsl_state) do
    with {:ok, action} <- get_action(dsl_state, destroy.resource, destroy.undo_action),
         :ok <- verify_is_create_action(dsl_state, destroy.resource, action) do
      verify_action_takes_record(dsl_state, destroy.resource, action)
    end
  end

  defp get_action(dsl_state, resource, action_name) do
    case Info.action(resource, action_name) do
      nil ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:destroy, :undo_action],
           message:
             "No action found matching the name `#{action_name}` on resource `#{inspect(resource)}`."
         )}

      action when is_struct(action) ->
        {:ok, action}
    end
  end

  defp verify_is_create_action(_dsl_state, _resource, action)
       when is_struct(action, Ash.Resource.Actions.Create),
       do: :ok

  defp verify_is_create_action(dsl_state, _resource, _action) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: [:destroy, :undo_action],
       message: "The undo action for an destroy step should be a create action."
     )}
  end

  defp verify_action_takes_record(_dsl_state, _resource, %{arguments: [%{name: :record}]}),
    do: :ok

  defp verify_action_takes_record(dsl_state, _resource, _action) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: [:destroy, :undo_action],
       message: "The undo action for an destroy step should take a single `record` argument."
     )}
  end
end
