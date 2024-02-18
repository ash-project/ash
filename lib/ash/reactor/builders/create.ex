defimpl Reactor.Dsl.Build, for: Ash.Reactor.Dsl.Create do
  @moduledoc false

  alias Ash.Reactor.CreateStep
  alias Ash.Resource.Info
  alias Reactor.Builder
  alias Spark.{Dsl.Transformer, Error.DslError}
  import Ash.Reactor.BuilderUtils

  @doc false
  @impl true
  def build(create, reactor) do
    with {:ok, reactor} <- ensure_hooked(reactor),
         {:ok, reactor, arguments} <- build_input_arguments(reactor, create) do
      arguments =
        arguments
        |> maybe_append(create.actor)
        |> maybe_append(create.tenant)
        |> Enum.concat(create.wait_for)

      action_options =
        create
        |> Map.take([
          :action,
          :api,
          :authorize?,
          :resource,
          :undo_action,
          :undo,
          :upsert_identity,
          :upsert?
        ])
        |> Enum.to_list()

      step_options =
        create
        |> Map.take([:async?])
        |> Map.put(:ref, :step_name)
        |> Enum.to_list()

      Builder.add_step(
        reactor,
        create.name,
        {CreateStep, action_options},
        arguments,
        step_options
      )
    end
  end

  @doc false
  @impl true
  def transform(_create, dsl_state), do: {:ok, dsl_state}

  @doc false
  @impl true
  def verify(create, _dsl_state) when create.undo == :never, do: :ok

  def verify(create, dsl_state) do
    case Info.action(create.resource, create.undo_action) do
      action when is_struct(action, Ash.Resource.Actions.Destroy) ->
        :ok

      nil ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:create, :undo_action],
           message:
             "No action found matching the name `#{create.undo_action}` on resource `#{inspect(create.resource)}`."
         )}

      _action ->
        {:error,
         DslError.exception(
           module: Transformer.get_persisted(dsl_state, :module),
           path: [:create, :undo_action],
           message: "The undo action for a create step should be a destroy."
         )}
    end
  end
end
