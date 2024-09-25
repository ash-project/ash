defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Update do
  @moduledoc false

  alias Ash.Reactor.UpdateStep
  alias Ash.Resource.Info
  alias Reactor.{Argument, Builder}
  alias Spark.{Dsl.Transformer, Error.DslError}
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(update, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, update) do
      arguments =
        arguments
        |> maybe_append(update.actor)
        |> maybe_append(update.tenant)
        |> maybe_append(update.load)
        |> maybe_append(update.context)
        |> Enum.concat(update.wait_for)
        |> Enum.concat([%Argument{name: :initial, source: update.initial}])

      action_options =
        update
        |> Map.take([:action, :authorize?, :domain, :resource, :undo, :undo_action])
        |> Enum.to_list()

      step_options =
        update
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        update.name,
        {UpdateStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def verify(update, _dsl_state) when update.undo == :never, do: :ok

  def verify(update, dsl_state) do
    with {:ok, action} <- get_action(dsl_state, update.resource, update.undo_action),
         :ok <- verify_is_update_action(dsl_state, update.resource, action) do
      verify_action_takes_changeset(dsl_state, update.resource, action)
    end
  end

  defp get_action(dsl_state, resource, action_name) do
    case Info.action(resource, action_name) do
      nil ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:update, :undo_action],
           message:
             "No action found matching the name `#{action_name}` on resource `#{inspect(resource)}`."
         )}

      action when is_struct(action) ->
        {:ok, action}
    end
  end

  defp verify_is_update_action(_dsl_state, _resource, action)
       when is_struct(action, Ash.Resource.Actions.Update),
       do: :ok

  defp verify_is_update_action(dsl_state, _resource, _action) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: [:update, :undo_action],
       message: "The undo action for an update step should also be an update."
     )}
  end

  defp verify_action_takes_changeset(_dsl_state, _resource, %{arguments: [%{name: :changeset}]}),
    do: :ok

  defp verify_action_takes_changeset(dsl_state, _resource, _action) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: [:update, :undo_action],
       message: "The undo action for an update step should take a single `changeset` argument."
     )}
  end
end
