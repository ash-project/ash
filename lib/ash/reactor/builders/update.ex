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
        |> Enum.concat(update.wait_for)
        |> Enum.concat([%Argument{name: :initial, source: update.initial}])

      action_options =
        update
        |> Map.take([:action, :api, :authorize?, :resource, :undo, :undo_action])
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
  def transform(_update, dsl_state), do: {:ok, dsl_state}

  @doc false
  @impl true
  def verify(update, _dsl_state) when update.undo == :never, do: :ok

  def verify(update, dsl_state) do
    case Info.action(update.resource, update.undo_action) do
      action when is_struct(action, Ash.Resource.Actions.Update) ->
        :ok

      nil ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:update, :undo_action],
           message:
             "No action found matching the name `#{update.undo_action}` on resource `#{inspect(update.resource)}`."
         )}

      _action ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:update, :undo_action],
           message: "The undo action for an update step should also be an update."
         )}
    end
  end
end
