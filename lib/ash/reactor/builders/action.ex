defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Action do
  @moduledoc false

  alias Ash.{Reactor.ActionStep, Resource.Info}
  alias Reactor.Builder
  alias Spark.{Dsl.Transformer, Error.DslError}

  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(action, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, action) do
      arguments =
        arguments
        |> maybe_append(action.actor)
        |> maybe_append(action.tenant)
        |> Enum.concat(action.wait_for)

      action_options =
        action
        |> Map.take([:action, :authorize?, :domain, :resource, :undo, :undo_action])
        |> Enum.to_list()

      step_options =
        action
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        action.name,
        {ActionStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def transform(_action, dsl_state), do: {:ok, dsl_state}

  @doc false
  @impl true
  def verify(action, _dsl_state) when action.undo == :never, do: :ok

  def verify(action, dsl_state) do
    case Info.action(action.resource, action.undo_action) do
      action when is_struct(action, Ash.Resource.Actions.Action) ->
        :ok

      nil ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:action, :undo_action],
           message:
             "No action found matching the name `#{action.undo_action}` on resource `#{inspect(action.resource)}`"
         )}

      _action ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:action, :undo_action],
           message: "The undo action for a generic action step should also be a generic action."
         )}
    end
  end
end
