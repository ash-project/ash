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
          :api,
          :authorize?,
          :resource,
          :return_destroyed?,
          :undo,
          :undo_action
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
  def verify(destroy, dsl_state) do
    verify_undo(destroy, dsl_state)
  end

  def verify_undo(destroy, _dsl_state) when destroy.undo == :never, do: :ok

  def verify_undo(destroy, dsl_state) do
    with :ok <- verify_returning_destroyed?(destroy, dsl_state) do
      verify_undo_action(destroy, dsl_state)
    end
  end

  defp verify_returning_destroyed?(destroy, _dsl_state) when destroy.returning_destroyed? == true,
    do: :ok

  defp verify_returning_destroyed?(_destroy, dsl_state) do
    {:error,
     DslError.exception(
       module: Transformer.get_persisted(dsl_state, :module),
       path: [:destroy, :return_destroyed?],
       message: "`return_destroyed?` must be true when undo is enabled."
     )}
  end

  defp verify_undo_action(destroy, dsl_state) do
    case Info.action(destroy.resource, destroy.undo_action) do
      action when is_struct(action, Ash.Resource.Actions.Create) ->
        :ok

      nil ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:destroy, :undo_action],
           message:
             "No action found matching the name `#{destroy.undo_action}` on resource `#{inspect(destroy.resource)}`."
         )}

      _action ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:destroy, :undo_action],
           message: "The undo action for a destroy step should be a create."
         )}
    end
  end
end
